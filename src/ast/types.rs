use super::base::*;
use crate::ast;

//#[derive(Debug)]
pub struct TypeReference<'a> {
    pub node_info: NodeInfo,

    pub typename: &'a str,
    pub type_parameters: Vec<TypeReference<'a>>,
    pub refers_to: Option<Box<dyn Type + Send>>,
    //pub span: Span<'a>,
}

impl<'a> std::fmt::Debug for TypeReference<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f, 0);

        write!(f, "")
    }
}

impl<'a> TypeReference<'a> {
    pub fn unit() -> TypeReference<'a> {
        /*let span = Span {
            start: 0,
            end: 0,
            literal: "()",
        };*/

        TypeReference {
            typename: "()",
            refers_to: None,
            node_info: ast::NodeInfo::Builtin,
            type_parameters: Vec::new(),
        }
    }

    /*pub fn from_name(name: &'a str) -> TypeReference<'a> {
        /*let span = Span {
            start,
            end,
            literal: "()",
        };*/

        TypeReference { failed: false, typename: name, refers_to: None }
    }*/

    /*pub fn from_token(input: &TokenWrapper<'a>) -> TypeReference<'a> {
        TypeReference {
            typename: input.slice,
            refers_to: None,
            node_info: ast::NodeInfo::from_token(input, true),
        }
    }*/
}

impl<'a> AstNode<'a> for TypeReference<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        let _ = write!(f, "{}<", self.typename);
        for tr in self.type_parameters.iter() {
            tr.display(f, depth);
            let _ = write!(f, ",");
        }
        let _ = write!(f, ">");
        /*let _ = writeln!(
        f,
        "{}Typereference with typename {} that refers to type {:?}",
        indent(depth),
        self.typename,
        self.refers_to,
        );*/
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

// marked as requiring Send and Sync because rayon parallel iterators require this to be the case,
// and this type is accessible through FileHandler indirectly
pub trait Type: std::fmt::Debug + std::marker::Send + std::marker::Sync /* don't ask */ {
    fn primitive(&self) -> bool {
        false
    }
}
