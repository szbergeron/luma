use super::expressions;
use crate::helper::lex_wrap::{CodeLocation, TokenWrapper};

pub fn indent(ind: usize) -> String {
    let mut s: String = "|".to_string();

    for _ in 0..ind {
        //s.push('\t');
        s.push_str("  ");
    }

    s
}

pub fn findent(f: &mut std::fmt::Formatter<'_>, depth: usize) {
    write!(f, "{}", indent(depth)).unwrap();
}

//pub type CodeLocation = Loc;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Span {
    pub start: CodeLocation,
    pub end: CodeLocation,
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.start, self.end)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum NodeInfo {
    Builtin,
    Parsed(ParsedNodeInfo),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ParsedNodeInfo {
    pub span: Span,
    //pub parsed: bool,
}

impl NodeInfo {
    pub fn as_parsed(&self) -> Option<&ParsedNodeInfo> {
        match self {
            Self::Builtin => None,
            Self::Parsed(info) => Some(info),
        }
    }

    pub fn from_token(input: &TokenWrapper) -> NodeInfo {
        let inner = ParsedNodeInfo {
            span: Span {
                start: input.start,
                end: input.end,
            },
            //parsed,
        };

        NodeInfo::Parsed(inner)
    }

    pub fn from_indices(start: CodeLocation, end: CodeLocation) -> NodeInfo {
        let inner = ParsedNodeInfo {
            span: Span { start, end },
            //parsed,
        };

        NodeInfo::Parsed(inner)
    }
}

impl std::fmt::Display for NodeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parsed(info) => write!(
                f,
                "over {}",
                /*if info.parsed {
                    "successfully"
                } else {
                    "unsuccessfully"
                },*/
                info.span
            ),
            Self::Builtin => write!(f, "buildin"),
        }
    }
}

#[allow(dead_code)]
pub trait AstNode: std::fmt::Debug + Send + Sync {
    fn node_info(&self) -> NodeInfo;
    //fn start(&self) -> CodeLocation;
    //fn end(&self) -> CodeLocation;

    /// Should display detailed debug info, node type, child info
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize);

    /// Should display as a "source like" form. May be parenthesized,
    /// and is allowed to include type information, but should
    /// omit things like location, parse success, strict node type and such
    ///
    /// Indentation for the first line is handled by the parent, but any newlines created inside
    /// should be indented by the child according to the `depth` parameter
    #[allow(unused_variables)]
    fn pretty(&self, f: &mut dyn std::fmt::Write, depth: usize) {
        panic!("[ast_prettyprint] not yet implemented for {}", std::any::type_name::<Self>());
    }

    fn start(&self) -> Option<CodeLocation> {
        self.node_info().as_parsed().map(|node| node.span.start)
    }

    fn end(&self) -> Option<CodeLocation> {
        self.node_info().as_parsed().map(|node| node.span.end)
    }

    fn as_expr(&self) -> Option<&mut dyn expressions::Expression> {
        None
    }

    //fn children(&self) -> [&'a mut dyn AstNode<'a>];
}

impl AstNode for Option<&dyn AstNode> {
    fn node_info(&self) -> NodeInfo {
        match self {
            Some(n) => n.node_info(),
            None => NodeInfo::Builtin,
        }
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) {
        match self {
            Some(n) => n.display(f, depth),
            None => write!(f, "<none>").unwrap(),
        }
    }

    fn pretty(&self, _f: &mut dyn std::fmt::Write, _depth: usize) {
        todo!("[ast_prettyprint]")
    }
}

impl IntoAstNode for Option<&dyn AstNode> {
    /*fn as_node_mut(&mut self) -> Option<&mut dyn AstNode> {
        Some(self)
    }*/

    fn as_node(&self) -> &dyn AstNode {
        self
    }
}

impl std::fmt::Display for &dyn AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(self.display(f, 0))
    }
}

/*impl std::fmt::Display for Option<&dyn AstNode> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            None => write!(f, "<unknown>"),
            Some(n) => n.fmt(f),
        }
    }
}*/

pub trait IntoAstNode {
    fn as_node(&self) -> &dyn AstNode;
}

/*impl<'a>  std::ops::Deref for dyn IntoAstNode<'a> {
    type Target = dyn AstNode<'a>;

    fn deref(&self) -> &'a Self::Target {
        self.as_node()
    }
}*/
