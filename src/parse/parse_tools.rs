//use crate::helper::lex_wrap::{LookaheadHandle};

use crate::lex::LookaheadHandle;

use super::Parser;

// During any recursive "rule", if we encounter
// a local parsing error we want to know what to synchronize to.
//
// A linked stack is formed at each stage, with each
// rule pushing a list of "next" items that it can try
// to consume.

#[derive(Clone, Copy)]
pub struct LexerStreamHandle {
    index: usize,
    id: usize,
}

pub struct ParseValueGuard<'tokens, ParseValue> {
    value: ParseValue,
    handle: LookaheadHandle<'tokens>,
}

impl<'tokens, T> ParseValueGuard<'tokens, T> {
    pub fn success<E>(
        value: T,
        handle: LookaheadHandle<'tokens>,
    ) -> Result<ParseValueGuard<'tokens, T>, E> {
        Ok(ParseValueGuard { value, handle })
    }

    pub fn open(self) -> (LookaheadHandle<'tokens>, T) {
        (self.handle, self.value)
    }
}

//pub type ParseResult<'t, T, E> = Result<ParseValueGuard<'t, T>, E>;

/**
 * A correction "match" can be represented as a point in a three dimensional cube
 *
 * One dimension is how "deep" into the chosen rule a correction goes
 * The second dimension is how many tokens needed to be dropped from the input stream
 *  before a rule that aligns can be found
 * The third dimension is how many parsing "blocks" need to be discarded before one with
 *  the chosen matching rule can be found
 *
 * When no errors are found, parsing basically just always exists at the "peak" of that cube,
 * where depth into the rule is 1, the current parsing block has the matching rule,
 * and no tokens from the input stream needed to be dropped.
 *
 * When errors occur, a search of that cube needs to occur. What we can preemptively do
 * here is to try to build one dimension of that cube as we go, so it is already memoized.
 * This aligns nicely with a recursive descent parser, as we can push which "block" a given
 * rule came from onto a stack. The search aims to try to find a solution point in 3d space
 * that minimizes distance from the origin. That distance has different weights applied to each
 * dimension, and tries to follow what a typical programmer does. Most often, especially in an
 * embedded environment like an editor, the programmer has forgotten or not yet typed a terminating
 * set of tokens. This could be a colon, comma, rbrace, or even longer omissions such as having
 * only added the opening elements of a struct or class and part of a member. Since so often errors are
 * in omission, we weight skipping tokens to be nearly the last corrective action that is taken.
 *
 * This algorithm is greedy, since it does not try to assess how well an edit will retain contiguity
 * of following elements. This means that it does not nicely solve having an "extra" closing brace without
 * a rather aggressive discard coefficient. It does solve missing a closing brace, however, as this
 * allows for a closing "insertion" before matching a following structure.
 *
 * This runs in worst-case r * d * n time, with a much more common r * d time assuming
 * errors tend to be by omission instead of being extraneous additional symbols.
 *
 * r is the maximum rule length of any rule in the language, d is the maximum depth of the language
 * (often log(n) but can theoretically be n in pathological cases), and n is the length of the
 * remaining input stream.
 *
 * d is actually somewhat a misnomer here since it is bounded by the grammar size.
 * If a rule reoccurs, then it would have been matched in an earlier call (or there
 * would have to have been an existing version of that rule with the same or lower
 * index to match). This means that d is bounded at g, which is the length of all rules
 * added together
 */
pub mod schema {

    use std::collections::HashMap;

    use smallvec::SmallVec;

    use crate::{
        ast::Span,
        lex::{ErrorSet, LookaheadHandle, ParseResultError, Token, TokenWrapper}, helper::EitherNone,
    };

    // This is intentionally very general, and doesn't give full flexibility to define
    // full regular rules. This allows for a more simple solver that doesn't have to
    // track loop counts or do breaking, and only introduces minimal error
    // in cases where things like "no trailing comma means a break" occur
    pub enum Nonterminal<'component> {
        /// Use for any actual token that should be matched imperatively
        Terminal(Token),

        /// Represents a child parsed rule. Not yet implemented
        Rule(&'static str), // use to represent a child parsed rule

        SubRule {
            range: Range,
            components: &'component [Nonterminal<'component>],
        },

        Split {
            variants: &'component [Nonterminal<'component>],
        },

        /// Represents the end of a block, either outer level or repeat
        End(),
    }

    impl<'component> Nonterminal<'component> {
        pub fn subrule<R>(rule: &'component [Nonterminal<'component>], range: R) -> Nonterminal<'component> where R: ToRange {
            Nonterminal::SubRule { range: range.to_range(), components: rule }
        }

        pub fn split(variants: &'static [Nonterminal<'component>]) -> Nonterminal<'component> {
            Nonterminal::Split { variants }
        }
    }

    // this can be composed by this syntax:
    // schema::unit(
    //     rule()
    // )

    enum Distance {
        Finite(usize),
        Infinite,
    }

    impl Distance {
        pub fn lesser(s: Self, o: Self) -> Self {
            match (s, o) {
                (Self::Infinite, Self::Infinite) => Self::Infinite,
                (Self::Finite(v1), Self::Finite(v2)) => Self::Finite(v1.min(v2)),
                (Self::Finite(v), Self::Infinite) | (Self::Infinite, Self::Finite(v)) => {
                    Self::Finite(v)
                }
            }
        }
    }

    type Range = (Option<isize>, Option<isize>);

    trait ToRange: Sized + Clone + Copy {
        fn to_range(self) -> (Option<isize>, Option<isize>);
    }

    impl ToRange for (Option<isize>, isize) {
        fn to_range(self) -> (Option<isize>, Option<isize>) {
            (self.0.unwrap_or(0), self.1).to_range()
        }
    }

    impl ToRange for (isize, Option<isize>) {
        fn to_range(self) -> (Option<isize>, Option<isize>) {
            (Some(self.0), self.1)
        }
    }

    impl ToRange for (isize, isize) {
        fn to_range(self) -> (Option<isize>, Option<isize>) {
            (Some(self.0), Some(self.1))
        }
    }

    impl ToRange for (Option<isize>, Option<isize>) {
        fn to_range(self) -> (Option<isize>, Option<isize>) {
            self
        }
    }

    /*pub struct Rule<'> {
        index: isize,
        subrules: HashMap<&'static str, &'static [Nonterminal]>,
        entry: &'static str,
        //tokens: &'static [Nonterminal],
    }

    impl Rule {
        pub fn new(tokens: &'static [Nonterminal]) -> Self {
            Self { index: 0, tokens }
        }

        fn rec_find(&self, i: usize, depth: usize, terminal: &[Token]) -> Distance {
            if depth > self.tokens.len() {
                Distance::Infinite
            } else {
                match terminal {
                    [] => Distance::Finite(0), // base case, we're already a match so no distance
                    [first, rest @ ..] => match self.tokens[i] {
                        Nonterminal::End() => Distance::Infinite,
                        //Nonterminal::Rule(_) => Distance::Infinite, // we're not going to currently try recursing
                        Nonterminal::Rule(_) => {
                            // we treat a rule as cost 1 for now
                            self.rec_find(i + 1, depth + 1, terminal)
                        }
                        Nonterminal::Terminal(t) => {
                            if t == *first {
                                self.rec_find(i + 1, depth, terminal)
                                //Distance::Finite(depth)
                            } else {
                                self.rec_find(i + 1, depth + 1, terminal)
                            }
                        }
                        Nonterminal::Repeat { index } => Distance::lesser(
                            self.rec_find(i + 1, depth + 1, terminal),
                            self.rec_find(index, depth + 1, terminal),
                        ),
                        Nonterminal::Skip { index } => Distance::lesser(
                            self.rec_find(i + 1, depth + 1, terminal),
                            self.rec_find(index, depth + 1, terminal),
                        ),
                    },
                }
            }
        }

        /*pub fn search_forward(&mut self, tw: TokenWrapper, mut index: usize) -> Option<isize> {
            loop {
                match self.tokens[index as usize] {
                    Nonterminal::Terminal(t) => {
                        if t == tw.token {
                            break Some(index + 1) // we now point to token directly after the matching terminal
                        } else {
                            break None
                        }
                    },
                    Nonterminal::End() => {
                        break None
                    },
                    _ => index += 1,
                }
            }
        }

        pub fn consumes(&mut self, tw: TokenWrapper) {
            match self.index {
                -1 => (), // do nothing, this arm/variant has been discarded
                other => {
                    match self.tokens[self.index] {
                        Nonterminal::End() => { // we don't accept further tokens, so much not be this rule
                            self.index = -1;
                        }
                        Nonterminal::Repeat { index } => {
                            // this algo is greedy, even if not entirely "correct". We try as soon
                            // as possible to advance, and only "go back" if the forward advance
                            // doesn't match
                            //
                            if let Some(idx) = self.search_forward(tw, self.index) {
                                self.index = idx;
                            } else if let Some(idx) = self.search_forward(tw, index)
                                self.index = idx;
                            } else {
                            }
                        }
                    }
                }
            }
        }*/

        pub fn distance(&self, t: Token) -> Distance {
            self.rec_find(0, 0, t)
        }
    }*/

    pub struct RecoveryToken {
        /// Gives the ID/index of the rule that was used to solve this recovery within
        /// the RuleUnit that matches_unit references
        matches_rule: usize,

        /// Refers to the ID of the RuleUnit that gave the most minimal correction action
        matches_unit: usize,

        /// When doing a synchronization action, this reports what area of the code had to be
        /// discarded (if any) to synchronize the stream. Most often this will be empty,
        /// and will simply be a zero-size range at the index of the token that caused the
        /// error cascade
        range_discarded: Span,
    }

    pub struct RuleUnit<'parent> {
        id: usize,
        parent: Option<&'parent RuleUnit<'parent>>,
        tokens: SmallVec<[Token; 10]>, // can potentially use an arena allocator or an ID reuse mechanism for this,
        // or just put it inline in the stack if we want to do unsized types
        // would depend on https://github.com/rust-lang/rust/issues/48055 for
        // support
        //
        // In meantime can likely use smallvec with maximum "normal" rule size
        encountered_tokens: SmallVec<[Token; 10]>,
    }

    impl<'parent> RuleUnit<'parent> {
        pub fn child(&'parent self) -> Self {
            RuleUnit {
                id: self.id + 1,
                parent: Some(&self),
                tokens: SmallVec::default(),
                encountered_tokens: SmallVec::default(),
            }
        }

        pub fn predict_next(&mut self, t: Token) {
            self.tokens.push(t);
        }

        pub fn predict(&mut self, t: &[Token]) {
            self.tokens.extend(t.iter().map(|&t| t));
        }

        pub fn consumes(&mut self, t: TokenWrapper) {
            match self.tokens.iter().enumerate().rfind(|(idx, tok)| **tok == t.token) {
                Some((idx, _)) => self.tokens.truncate(idx),
                None => (),
            }
            //self.encountered_tokens.push(t.token);
        }

        fn search_internal(&self, t: TokenWrapper, position: isize, lh: &LookaheadHandle) -> Option<Solution> {
            match self.tokens.iter().rposition(|&tok| tok == t.token) {
                Some(pos) => {
                    for v in self.tokens[pos..].iter().rev() {
                        self.encountered_tokens.push(*v);
                    }

                    //use crate::ast::Span;

                    Some(Solution {
                        solution_unit_id: self.id,
                        solution_rule_id: 0,
                        jump_to: position,
                        range_discarded: Span { start: lh.la(0).unwrap().start, end: t.end }
                    })
                },
                None => {
                    self.parent.map(|p| p.search_internal(t, position, lh)).flatten()
                }
            }
        }

        pub fn search(&self, lh: &LookaheadHandle) -> Option<Solution> {
            for i in 0.. {
                match lh.la(i) {
                    Ok(tw) => {
                        match self.search_internal(tw, i, lh) {
                            Some(solution) => return Some(solution),
                            None => continue,
                        }
                    },
                    Err(_) => return None
                }
            }

            panic!()
        }

        /*pub fn search(&self, tw: TokenWrapper, lh: &LookaheadHandle) -> Option<Solution> {
            // need to find candidate rules that would allow getting to this state
            let candidates = self
                .rules
                .iter()
                .filter(|rule| rule.is_candidate(tw.token, &self.encountered_tokens));
            panic!()
        }*/
    }

    pub struct CorrectingHandle<'tokenvec> {
        tokens: &'tokenvec Vec<TokenWrapper>,
        index: usize,
    }

    /// A corrector acts to "dole out" tokens to requesting parsers.
    ///
    /// Effectively, every "request" for a token is saying that if the current
    /// ErrorHandle is eligible to receive a token (no error correction is occurring or the handle
    /// is from a nonterminal that has been found to be a recovery point) *and* the requested token
    /// is in the stream then that token will be given out.
    ///
    /// Correctors are interior-mutable
    pub struct Corrector {
        //
    }

    pub struct TokenProvider<'parent, 'tokens> {
        unit_rules: RuleUnit<'parent>,
        //parent: Option<&'parent TokenProvider<'parent, 'tokens>>,
        lh: LookaheadHandle<'tokens>,
    }

    type ParseResult<T> = Result<EitherNone<T, ErrorSet>, CorrectionBubblingError>;

    type TokenResult = ParseResult<TokenWrapper>;

    impl<'parent, 'tokens> TokenProvider<'parent, 'tokens> {
        pub fn child(&'parent self) -> TokenProvider<'parent, 'tokens> {
            Self {
                //parent: Some(&self),
                unit_rules: self.unit_rules.child(),
                lh: self.lh.clone(),
            }
        }

        pub fn provides(&self, s: Solution) -> bool {
            s.solution_unit_id == self.unit_rules.id
        }

        pub fn try_take(&mut self, t: Token) -> Option<TokenWrapper> {
            self.try_take_in(&[t])
        }

        pub fn try_take_in(&mut self, t: &[Token]) -> Option<TokenWrapper> {
            let nt = self.lh.la(0);

            match nt {
                Err(_) => None,
                Ok(nt) => {
                    if t.contains(&nt.token) {
                        self.lh.advance();
                        Some(nt)
                    } else {
                        None
                    }
                }
            }
        }

        pub fn take(&mut self, t: Token) -> TokenResult {
            self.take_in(&[t])
        }

        pub fn predict(self, t: &[Token]) -> Self {
            self.unit_rules.predict(t);
            self
        }

        pub fn predict_next(&mut self, t: Token) {
            self.unit_rules.predict_next(t);
        }

        pub fn take_in(&mut self, t: &[Token]) -> TokenResult {
            match self.peek_for_in(t) {
                Some(tw) => {
                    self.unit_rules.consumes(tw);
                    Ok(EitherNone::A(tw))
                }
                None => {
                    // need to do a search and potentially issue a bubbling error
                    let tr = self.lh.la(0);

                    // if the stream is just empty then that isn't a bubbling error,
                    // that's just a plain error
                    match tr {
                        Err(_le) => {
                            let mut ev = ErrorSet::new();
                            ev.push(ParseResultError::EndOfFile);
                            TokenResult::Ok(EitherNone::B(ev))
                        }
                        Ok(tw) => {
                            // it didn't directly match one of the requested tokens,
                            // so we should start a bubbling cascade
                            let solution = self.unit_rules.search(&self.lh);

                            let mut es = ErrorSet::new();
                            es.push(ParseResultError::UnexpectedToken(tw, t.to_vec(), None));

                            let e = es;

                            let bubbling = CorrectionBubblingError {
                                internal_error: e,
                                solution,
                            };

                            Err(bubbling)
                        }
                    }
                }
            }
        }

        pub fn peek_for(&self, t: Token) -> Option<TokenWrapper> {
            self.peek_for_in(&[t])
        }

        pub fn peek_for_in(&self, t: &[Token]) -> Option<TokenWrapper> {
            let nt = self.lh.la(0);

            match nt {
                Err(_) => None,
                Ok(nt) => {
                    if t.contains(&nt.token) {
                        Some(nt)
                    } else {
                        None
                    }
                }
            }
        }
    }

    /*trait CorrectionBubblingResult where Self: Sized {
        fn catch(self, context: &TokenProvider) -> Self {
            //
        }
    }*/

    /// Describes a recovery solution
    #[derive(Clone, Copy, Debug)]
    pub struct Solution {
        /// The id of the unit that this solution solves for
        solution_unit_id: usize,

        /// The rule that this solution matches for that unit
        solution_rule_id: usize,

        /// The position in the input stream this solution moves the
        /// consumption cursor to
        jump_to: isize,
        ///
        /// When doing a synchronization action, this reports what area of the code had to be
        /// discarded (if any) to synchronize the stream. Most often this will be empty,
        /// and will simply be a zero-size range at the index of the token that caused the
        /// error cascade
        range_discarded: Span,
        /*
        /// Any messages that are collected, stating what was being expected at the time
        /// as well as additional info for the solve
        messages: SmallVec<[String; 3]>,
        */
    }

    pub trait ResultHint {
        fn hint(self, hint: &'static str) -> Self;
    }

    impl<T> ResultHint for Result<T, ErrorSet> {
        fn hint(self, hint: &'static str) -> Self {
            self.map_err(|pre| {
                ParseResultError::ErrorWithHint {
                    hint,
                    original: Box::new(pre),
                }
                .as_set()
            })
        }
    }

    impl<T> ResultHint for Result<Result<T, ErrorSet>, CorrectionBubblingError> {
        fn hint(self, hint: &'static str) -> Self {
            match self {
                Ok(r) => Ok(r.hint(hint)),
                Err(cbe) => Err(CorrectionBubblingError {
                    solution: cbe.solution,
                    internal_error: ParseResultError::ErrorWithHint {
                        hint,
                        original: Box::new(cbe.internal_error),
                    }
                    .as_set(),
                }),
            }
        }
    }

    pub struct CorrectionBubblingError {
        /// A CBE is always caused by an underlying
        /// parse error. That should go here
        internal_error: ErrorSet,

        /// If this error was possible to resynchronize to the input stream,
        solution: Option<Solution>,
    }

    trait CorrectionBubblingResult<V> {
        fn catch(self, t: &TokenProvider) -> Result<Result<V, ErrorSet>, CorrectionBubblingError>;
    }

    impl<V> CorrectionBubblingResult<V> for Result<V, CorrectionBubblingError> {
        fn catch(self, t: &TokenProvider) -> Result<Result<V, ErrorSet>, CorrectionBubblingError> {
            match self {
                Err(cbe) => {
                    match cbe
                        .solution
                        .map(|solution| t.provides(solution))
                        .unwrap_or(false)
                    {
                        true => Ok(Err(cbe.internal_error)),
                        false => Err(cbe),
                    }
                }
                Ok(any) => Ok(Ok(any)),
            }
        }
    }

    /*impl<V> std::ops::Try for CorrectionBubblingResult<V> {
        type Output = V;
        type Residual = Self;
    }*/
}
