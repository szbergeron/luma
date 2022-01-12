use crate::helper::interner::IStr;

use super::{AstNode, IntoAstNode, NodeInfo, ScopedNameReference};

//use crate::types::FunctionDeclaration;

#[derive(Debug, Clone)]
pub struct ImplementationBody {
    fields: Vec<FieldMember>,
    //methods: Vec<FunctionDeclaration>,
    //aliases: Vec<AliasDeclaration>,
}

#[derive(Debug, Clone)]
pub struct MemberAttributes {
    pub public: bool,
    pub mutable: bool,
    //deferred: bool,
}

#[derive(Debug, Clone)]
pub struct FieldMember {
    pub attributes: MemberAttributes,

    pub name: IStr,

    pub ftype: TypeReference,
}

/// These act like structs or records in any other language,
/// Member functions here (once implemented) are devirtualized by default
///
/// This is a product type
#[derive(Debug, Clone)]
pub struct RecordValueDefinition {
    pub node_info: NodeInfo,
    pub fields: Vec<FieldMember>,
}

/// Not yet implemented, but will allow for typical pattern matching
/// later.
///
/// This is a sum type
#[derive(Debug, Clone)]
pub struct EnumValueDefinition {}

/// These are used for either record or enum types, and allow adding
/// immutable, replaceable, vtables to types
#[derive(Debug, Clone)]
pub struct RecordVirtualDefinition {
    pub node_info: NodeInfo,
    pub attrs: MemberAttributes,
    //generic_params: Vec<IStr>,
    pub implements_type: Option<TypeReference>,
    pub for_type: TypeReference,
    pub body: ImplementationBody,
}

/// Allows specifying a type schema for virtual record types
#[derive(Debug, Clone)]
pub struct RecordVirtualSpecification {}

impl AstNode for RecordVirtualSpecification {
    fn node_info(&self) -> NodeInfo {
        todo!()
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        todo!()
    }
}

impl AstNode for RecordValueDefinition {
    fn node_info(&self) -> NodeInfo {
        todo!()
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        todo!()
    }
}

impl AstNode for EnumValueDefinition {
    fn node_info(&self) -> NodeInfo {
        todo!()
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        todo!()
    }
}

impl AstNode for RecordVirtualDefinition {
    fn node_info(&self) -> NodeInfo {
        todo!()
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum TypeDefinition {
    Struct(RecordValueDefinition),
    Enum(EnumValueDefinition),
    Specification(RecordVirtualSpecification),
    Implementation(RecordVirtualDefinition),
}

impl AstNode for TypeDefinition {
    fn node_info(&self) -> NodeInfo {
        match self {
            Self::Struct(s) => s.node_info(),
            Self::Enum(s) => s.node_info(),
            Self::Specification(s) => s.node_info(),
            Self::Implementation(s) => s.node_info(),
        }
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct TypeReference {
    node_info: NodeInfo,

    pub ctx: ScopedNameReference,
    pub canonicalized_name: IStr,

    pub type_args: Vec<TypeReference>,
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
