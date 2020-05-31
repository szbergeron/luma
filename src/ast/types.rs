use super::base::*;
use crate::helper::lex_wrap::TokenWrapper;
use crate::ast;

#[derive(Debug)]
pub struct TypeReference<'a> {
    pub node_info: NodeInfo,

    pub typename: &'a str,
    pub refers_to: Option<Box<dyn Type>>,
    //pub span: Span<'a>,
}

impl<'a> TypeReference<'a> {
    pub fn unit() -> TypeReference<'a> {
        /*let span = Span {
            start: 0,
            end: 0,
            literal: "()",
        };*/

        TypeReference { typename: "()", refers_to: None, node_info: ast::NodeInfo::Builtin }
    }

    /*pub fn from_name(name: &'a str) -> TypeReference<'a> {
        /*let span = Span {
            start,
            end,
            literal: "()",
        };*/

        TypeReference { failed: false, typename: name, refers_to: None }
    }*/

    pub fn from_token(input: &TokenWrapper<'a>) -> TypeReference<'a> {
        TypeReference {
            typename: input.slice,
            refers_to: None,
            node_info: ast::NodeInfo::from_token(input, true),
        }
    }
}

impl<'a> AstNode<'a> for TypeReference<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = writeln!(
            f,
            "{}Typereference with typename {} that refers to type {:?}",
            indent(depth),
            self.typename,
            self.refers_to,
            );
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

pub trait Type: std::fmt::Debug {
    fn primitive(&self) -> bool {
        false
    }
}
