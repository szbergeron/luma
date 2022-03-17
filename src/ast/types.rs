use crate::{
    ast::indent,
    helper::interner::{IStr, SpurHelper},
};

use super::{AstNode, ExpressionWrapper, IntoAstNode, NodeInfo, ScopedNameReference};

//use crate::types::FunctionDeclaration;

#[derive(Debug, Clone)]
pub struct DefinitionBody {
    fields: Vec<FieldMember>,
    //methods: Vec<FunctionDeclaration>,
    //aliases: Vec<AliasDeclaration>,
}

#[derive(Debug, Clone)]
pub enum ImplementationItem {
    Function(FunctionDefinition),
    Field(FieldMember),
}

#[derive(Debug, Clone)]
pub struct ImplementationBody {
    pub node_info: NodeInfo,
    pub items: Vec<ImplementationItem>,
}

#[derive(Debug, Clone)]
pub struct Implementation {
    node_info: NodeInfo,

    body: ImplementationBody,

    impl_of: TypeReference,

    impl_for: TypeReference,
}

impl AstNode for Implementation {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug, Clone)]
pub struct MemberAttributes {
    pub public: bool,
    pub mutable: bool,
    //deferred: bool,
}

#[derive(Debug, Clone)]
pub struct FieldMember {
    //pub attributes: MemberAttributes,
    pub name: IStr,

    pub ftype: TypeReference,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub node_info: NodeInfo,

    pub public: bool,
    pub name: IStr,

    pub body: Box<ExpressionWrapper>,
    pub return_type: TypeReference,
    //pub params: Vec<(Box<super::ExpressionWrapper>, super::TypeReference)>,
    pub params: Vec<(IStr, super::TypeReference)>,
}

impl AstNode for FunctionDefinition {
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        let _ = write!(f, "fn {} (", self.name.resolve(),);
        for p in self.params.iter() {
            let _ = write!(f, "{}:", p.0.resolve());
            p.1.pretty(f, depth + 1);
            let _ = write!(f, ", ");
        }
        let _ = write!(f, ") -> ");
        self.return_type.pretty(f, depth + 1);
        let _ = write!(f, " ");
        //let _ =
        //let _ = writeln!(f, " {{");
        //let _ = write!(f, "{}", indent(depth + 1));

        self.body.as_node().pretty(f, depth);
        //let _ = writeln!(f, "");
        //let _ = writeln!(f, "\n{}}}", indent(depth));
    }
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    /*fn format(&self) -> RcDoc {
        let r = RcDoc::text("FunctionDeclaration with name ")
            .append(self.name.resolve())
            .append(RcDoc::line());

        let r = r
            .append("Parameters: ")
            .append(
                RcDoc::intersperse(
                    self.params
                        .iter()
                        .map(|(name, tr)| RcDoc::text(name.resolve()).append(": ").append(tr.format())),
                    comma_break(),
                )
                .nest(1),
            )
            .append(RcDoc::line());

        let r = r
            .append("Returns: ")
            .append(self.return_type.format().nest(1))
            .append(RcDoc::line());

        let r = r
            .append("Body:")
            .append(self.body.as_node().format().nest(1));

        r
    }*/
}

impl AstNode for FieldMember {
    fn node_info(&self) -> NodeInfo {
        todo!()
    }
}

/// These act like structs or records in any other language,
/// Member functions here (once implemented) are devirtualized by default
///
/// This is a product type
#[derive(Debug, Clone)]
pub struct StructDefinition {
    pub name: IStr,
    pub node_info: NodeInfo,
    pub fields: Vec<FieldMember>,
}

/// Not yet implemented, but will allow for typical pattern matching
/// later.
///
/// This is a sum type
#[derive(Debug, Clone)]
pub struct EnumDefinition {
    no: !,
}

/// These are used for either record or enum types, and allow adding
/// immutable, replaceable, vtables to types
#[derive(Debug, Clone)]
pub struct TraitDefinition {
    pub node_info: NodeInfo,
    pub attrs: MemberAttributes,
    //generic_params: Vec<IStr>,
    pub implements_type: Option<TypeReference>,
    pub for_type: TypeReference,
    pub body: DefinitionBody,
}

/// Allows specifying a type schema for virtual record types
#[derive(Debug, Clone)]
pub struct RecordVirtualSpecification {}

impl AstNode for RecordVirtualSpecification {
    fn node_info(&self) -> NodeInfo {
        todo!()
    }
}

impl AstNode for StructDefinition {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }

    /*fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        writeln!(f, "{} struct {} {{", indent(depth), self.name.resolve());
        for field in self.fields {
        }
        writeln!(f, "{}}}", indent(depth));
    }*/
}

impl AstNode for EnumDefinition {
    fn node_info(&self) -> NodeInfo {
        todo!()
    }
}

impl AstNode for TraitDefinition {
    fn node_info(&self) -> NodeInfo {
        self.node_info
    }
}

#[derive(Debug, Clone)]
pub enum TypeDefinition {
    Struct(StructDefinition),
    Enum(EnumDefinition),
    Specification(RecordVirtualSpecification),
    Implementation(TraitDefinition),
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
}

impl IntoAstNode for TypeDefinition {
    fn as_node(&self) -> &dyn AstNode {
        match self {
            Self::Struct(e) => e,
            Self::Enum(e) => e,
            Self::Specification(e) => e,
            Self::Implementation(e) => e,
        }
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
    /*fn display(&self, f: &mut std::fmt::Formatter<'_>, _depth: usize) {
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
    }*/

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
