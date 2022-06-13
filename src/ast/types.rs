use crate::{
    ast::indent,
    helper::interner::{IStr, SpurHelper},
};

use super::{AstNode, ExpressionWrapper, IntoAstNode, NodeInfo, ScopedNameReference};

//use crate::types::FunctionDeclaration;
//

pub mod types {
    /// A TypeReference represents the sort of thing that goes directly after declaring a variable,
    /// or for a function parameter. At parse time and AST construction, it is unresolved and
    /// the only information it has is the canonicalized type string and the scope that it attempts
    /// to reference. It is resolved (and modified using interior mutability) to the type that it
    /// resolves to during a compiler pass, which holds a reference to a constraint, and possibly
    /// an actual TypeID or TypeHandle
    #[derive(Debug)]
    pub struct TypeReference {
        resolution: std::sync::RwLock<TypeResolution>,
        syntax: super::syntax::TypeReference,
    }

    impl Clone for TypeReference {
        fn clone(&self) -> Self {
            Self {
                syntax: self.syntax.clone(),
                resolution: std::sync::RwLock::new(self.resolution.read().unwrap().clone()),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum TypeResolution {
        Unresolved(),
        Resolved(std::sync::Arc<TypeConstraint>),
    }

    /// Every type reference is effectively a constraint on what that variable can be.
    #[derive(Debug, Clone)]
    pub enum TypeConstraint {
        /// A "narrowing" constraint is one where a partial reference is given.
        /// This could be in the case where a variable is being assigned from a function and we
        /// want to hold it as some type, or where we are taking a literal type and
        /// provide a "u128" constraint to the variable it's going in to which backpropagates
        /// and changes what type the literal appears as
        ///
        /// It also means that if we have a variable of type T, but it comes from a source of T+U,
        /// that variable itself is only T rather than being T+U from the type system perspective.
        ///
        /// Accessing U again requires a narrowing conversion with some (T) -> Maybe<T+U>
        Widening(),

        /// A composite constraint is one that inherits the constraints of its uses. This
        /// corresponds to "wildcard" or other non-constraints, such as implicit constraints within
        /// an expression. If we have a function that returns T+U, save it into a _ variable, and
        /// then only pass that variable into a (U) -> ?, then _ is resolved to U. If we try to
        /// pass a _ variable from a T+U function into a (V) -> ?, then the composite is
        /// unresolvable and the compiler throws an error
        Composite(),

        /// A source constraint is an absolute constraint. This appears in cases like the
        /// post-generic-resolution signature of a function. This is also used for global
        /// constants.
        Source(),
    }

    use crate::{helper::interner::IStr, ast::ExpressionWrapper};

    #[derive(Debug, Clone)]
    /// A fieldmember can be either a method or a sized field.
    /// For value types within the inherent impl context, fields exist within
    /// the structural record and methods are fully devirtualized.
    ///
    /// For virtual types, fieldmembers exist as entries within the virttype vtable whether
    /// they are methods or variables.
    pub struct FieldMember {
        /// Every attribute of a type is named (even if not uniquely).
        pub name: IStr,

        /// Every field is uniquely described by the pair of name and type
        pub ftype: TypeReference,

        /// Any field may have a provided default value for a member.
        ///
        /// Struct inherent implementations of functions demand default falues,
        /// but virtual types do not. Every other (known) instance of FieldMember does not require
        /// any default literal value
        ///
        /// A default value is not really a "value", but is instead implicitly a callable with the
        /// contents wrapped in a parameterless closure. These closures have static constants all
        /// visible to them, as well as typical literals and all imported constants.
        pub default: BasicClosure,
    }

    /// A VirtualType is an interface or other non-structured type. It can be inherently
    /// implemented or runtime composed (and used through a CompositeType).
    #[derive(Debug, Clone)]
    pub struct VirtualType {
        members: Vec<FieldMember>,
    }

    #[derive(Debug, Clone)]
    pub struct StructType {
        members: Vec<FieldMember>,
    }

    /*pub trait PrimitiveType: std::fmt::Debug + Clone {
    }*/

    #[derive(Debug, Clone)]
    pub enum PrimitiveType {
        //
    }

    /// If a type is an enum, struct, or primitive, then it is a ValueType.
    #[derive(Debug, Clone)]
    pub enum ValueType {
        Callable(CallableType),
        Struct(StructType),
        Enum(!),
        Primitive(PrimitiveType),
    }

    /// A composed type is one that only exists at usage sites, and
    /// embodies types of the form "T + U". This is not used for types that inherit
    /// or have inherent implementations of traits, as those are natively part of the ValueType or
    /// VirtualType. This class of type only exists because of the runtime type composition
    /// features of LumaLang.
    #[derive(Debug, Clone)]
    pub struct ComposedType {
        member_types: Vec<TypeReference>,
    }

    #[derive(Debug, Clone)]
    pub struct BasicClosure {
        signature: ValueType, // always a CallableType
        
        arguments: Vec<IStr>,

        code: Box<ExpressionWrapper>,
    }

    /*pub trait Type {
        fn fields(&self) -> Vec<&FieldMember>;

        fn methods(&self) -> Vec<&FieldMember>;
    }*/

    /// A RealType is a fully resolved type with all generics resolved.
    pub enum RealType {
        Value(ValueType),
        Virtual(VirtualType),
    }

    pub struct TypeAlias {
        unimplemented: !,
    }

    pub enum Type {
        Composed(ComposedType),
        Real(RealType),
        Alias(TypeAlias),
    }
}

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
    pub node_info: NodeInfo,

    pub body: ImplementationBody,

    pub impl_of: Option<TypeReference>,

    pub impl_for: TypeReference,
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

pub mod syntax {
    use crate::ast::{ScopedNameReference, NodeInfo, AstNode, IntoAstNode};
    use crate::{
        ast::indent,
        helper::interner::{IStr, SpurHelper},
    };

    #[derive(Debug, Clone)]
    pub struct TypeReference {
        node_info: NodeInfo,

        pub ctx: ScopedNameReference,
        pub canonicalized_name: IStr,

        pub type_args: Vec<TypeReference>,
    }

    impl TypeReference {
        pub fn new(ctx: ScopedNameReference, name: IStr, node_info: NodeInfo) -> TypeReference {
            TypeReference {
                node_info,
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
}
