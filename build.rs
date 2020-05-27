extern crate lalrpop;

fn main() {
    //lalrpop::process_root().expect("Build failed: generating parser failed");
    lalrpop::process_root().expect("LALRPop parser generation failed");
}
