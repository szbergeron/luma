pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub enum Tok {

