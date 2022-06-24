use crate::cst::CstNode;

trait Walker {
    fn walk_root(root: &mut dyn CstNode);
}
