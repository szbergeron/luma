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

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: CodeLocation,
    pub end: CodeLocation,
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.start, self.end)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum NodeInfo {
    Builtin,
    Parsed(ParsedNodeInfo),
}

#[derive(Debug, Clone, Copy)]
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

pub trait AstNode: std::fmt::Debug + Send + Sync {
    fn node_info(&self) -> NodeInfo;
    //fn start(&self) -> CodeLocation;
    //fn end(&self) -> CodeLocation;
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize);

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

pub trait IntoAstNode {
    fn as_node_mut(&mut self) -> &mut dyn AstNode;
    fn as_node(&self) -> &dyn AstNode;
}

/*impl<'a>  std::ops::Deref for dyn IntoAstNode<'a> {
    type Target = dyn AstNode<'a>;

    fn deref(&self) -> &'a Self::Target {
        self.as_node()
    }
}*/
