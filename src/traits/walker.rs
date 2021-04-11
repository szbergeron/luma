use crate::ast::AstNode;

trait Walker {
    fn walk_root(root: &mut dyn AstNode);
}
