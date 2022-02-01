use crate::helper::lex_wrap::{TokenWrapper, LookaheadHandle};

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
    pub fn success<E>(value: T, handle: LookaheadHandle<'tokens>) -> Result<ParseValueGuard<'tokens, T>, E> {
        Ok(ParseValueGuard { value, handle })
    }

    pub fn open(self) -> (LookaheadHandle<'tokens>, T) {
        (self.handle, self.value)
    }
}

pub type ParseResult<'t, T, E> = Result<ParseValueGuard<'t, T>, E>;

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

    use crate::lex::Token;

    // This is intentionally very general, and doesn't give full flexibility to define
    // full regular rules. This allows for a more simple solver that doesn't have to
    // track loop counts or do breaking, and only introduces minimal error
    // in cases where things like "no trailing comma means a break" occur
    pub enum Nonterminal {
        Repeat { index: usize },
        Terminal(Token),
        Rule(&'static str), // use to represent a child parsed rule
        End(),
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
                (Self::Finite(v), Self::Infinite) | (Self::Infinite, Self::Finite(v)) => Self::Finite(v)
            }
        }
    }

    pub struct Rule {
        index: usize,
        tokens: &'static [Nonterminal],
    }
    
    impl Rule {
        pub fn new(tokens: &'static [Nonterminal]) -> Self {
            Self { index: 0, tokens }
        }

        fn rec_find(&self, i: usize, depth: usize, terminal: Token) -> Distance {
            if depth > self.tokens.len() {
                Distance::Infinite
            } else {
                match self.tokens[i] {
                    Nonterminal::End() => Distance::Infinite,
                    Nonterminal::Rule(_) => Distance::Infinite, // we're not going to currently try recursing
                    Nonterminal::Terminal(t) => if t == terminal { Distance::Finite(depth) } else { self.rec_find(i + 1, depth + 1, terminal)}
                    Nonterminal::Repeat { index } => {
                        Distance::lesser(self.rec_find(i + 1, depth + 1, terminal), self.rec_find(index, depth + 1, terminal))
                    }
                }
            }

        }
        
        pub fn distance(&self, t: Token) -> Distance {
            self.rec_find(0, 0, t)
        }
    }

    pub struct SchemaUnit<'parent> {
    }

    /// A corrector acts to "dole out" tokens to requesting parsers.
    ///
    /// Effectively, every "request" for a token is saying that if the current
    /// ErrorHandle is eligible to receive a token (no error correction is occurring or the handle
    /// is from a nonterminal that has been found to be a recovery point) *and* the requested token
    /// is in the stream then that token will be given out. 
    pub struct Corrector {
    }
}
