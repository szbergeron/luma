use crate::helper::interner::IStr;

use super::{AstNode, IntoAstNode, NodeInfo, ScopedNameReference};

//use crate::types::FunctionDeclaration;

pub struct TypeBlock {
    fields: Vec<FieldDeclaration>,
    methods: Vec<FunctionDeclaration>,
    //aliases: Vec<AliasDeclaration>,
}

pub struct MemberAttributes {
    public: bool,
    mutable: bool,
    //deferred: bool,
}

pub struct FieldDeclaration {
    attributes: MemberAttributes,

    name: IStr,

    ftype: TypeReference,
}

pub struct FunctionDeclaration {
    attributes: MemberAttributes,
}

#[derive(Debug, Clone)]
pub struct TypeReference {
    node_info: NodeInfo,

    pub ctx: ScopedNameReference,
    pub canonicalized_name: IStr,

    pub type_args: Vec<Box<TypeReference>>,
}

impl TypeReference {
    pub fn new(ctx: ScopedNameReference, name: IStr) -> TypeReference {
        TypeReference {
            node_info: NodeInfo::Builtin,
            ctx,
            type_args: Vec::new(),
            canonicalized_name: name,
        }
    }
}

impl AstNode for TypeReference {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = write!(f, "{}", self.canonicalized_name);
        if !self.type_args.is_empty() {
            let _ = write!(f, "<args: !impl>");
        }
    }
    fn display(&self, f: &mut std::fmt::Formatter<'_>, _depth: usize) {
        let _ = write!(
            f,
            "{}",
            self.ctx.as_node(),
            //self.canonicalized_name.resolve() //interner().resolve(&self.canonicalized_name)
        );
        if !self.type_args.is_empty() {
            write!(f, "<").unwrap();
            for idx in 0..self.type_args.len() {
                write!(f, "{}", self.type_args[idx].as_node()).unwrap();
                if idx < self.type_args.len() - 1 {
                    write!(f, ", ").unwrap();
                }
            }
            write!(f, ">").unwrap();
        }
        /*
        [&self.subexpr]
            .iter()
            .for_each(|expr| expr.as_node().display(f, depth + 1));*/
    }

    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

}

impl IntoAstNode for TypeReference {
    /*fn as_node_mut(&mut self) -> &mut dyn AstNode {
        self
    }*/

    fn as_node(&self) -> &dyn AstNode {
        self
    }
}
