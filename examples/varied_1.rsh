mod let {
}

mod let {
}

mod Outer {
    pub fn parse(self: Parser<String>, input: String) -> Tuple2<AST, String /* remainder */> {

        let f = 5;

        let (rhs, remainder) = atomic(input);

        while let (tok, slice) = self.next() {
            //let (tok, slice) = self.next();

            if tok == Token::Number {
                return Result::Error(ParseError::UnexpectedToken(tok));
            } else if tok.is_operator() {
                rhs = BinaryOperator(tok, rhs, parse(self, input));
            }
        }
    };

    let f = (4, 5 * 2);
}
