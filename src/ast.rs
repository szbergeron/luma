pub mod constants {
    // prefix UQ indicates UNQUALIFIED
    const UQ_OPERATOR_MUL: &str = "unqualified_operator_multiply"; // *
    const UQ_OPERATOR_DIV: &str = "unqualified_operator_divide"; // /
    const UQ_OPERATOR_SUB: &str = "unqualified_operator_subtract"; // -
    const UQ_OPERATOR_ADD: &str = "unqualified_operator_add"; // +
    const UQ_OPERATOR_EXP: &str = "unqualified_operator_exponentiate"; // ^

    //const UQ_COERCE_
}

/*pub mod literal {
    pub enum */

#[derive(Debug)]
pub struct Namespace<'a> {
    // maybe add a <symbols> member for better perf checking
    // redeclaration?
    pub name: Option<&'a str>,
    pub contents: ParseUnit<'a>,
}

#[derive(Debug)]
pub struct ParseUnit<'a> {
    //namespaces: Vec<Namespace<'a>>,
    pub declarations: Vec<SymbolDeclaration<'a>>,
    // modrefs (using exprs)?
}

#[derive(Debug)]
pub enum SymbolDeclaration<'a> {
    FunctionDeclaration(FunctionDeclaration<'a>),
    StaticDeclaration(VariableDeclaration<'a>),
    NamespaceDeclaration(Namespace<'a>),
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub expressions: Vec<Box<dyn Expression>>,
    pub return_type: TypeReference<'a>,
    pub params: Vec<VariableDeclaration<'a>>,
}

#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub name: &'a str,
    pub var_type: Option<TypeReference<'a>>, // None indicates request for type inference
}

/*pub struct ParseUnit {
    namespace: Namespace,
}*/

#[derive(Debug)]
pub struct TypeReference<'a> {
    typename: &'a str,
    refers_to: Option<Box<dyn Type>>,
}

pub trait Type: std::fmt::Debug {
}

pub trait Expression: std::fmt::Debug {
}
