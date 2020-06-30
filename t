[1mdiff --git a/src/compile/stager.rs b/src/compile/stager.rs[m
[1mindex 84d9f8b..33dd875 100644[m
[1m--- a/src/compile/stager.rs[m
[1m+++ b/src/compile/stager.rs[m
[36m@@ -80,6 +80,80 @@[m [mpub struct CFlags {[m
 }[m
 [m
 pub fn launch(args: &[&str]) {[m
[32m+[m[32m    let args = parse_args(args).expect("couldn't parse arguments");[m
[32m+[m
[32m+[m[32m    let mut path_map = PathIdMap::new();[m
[32m+[m[32m    let mut scope_map = ScopeIdMap::new();[m
[32m+[m
[32m+[m[32m    let (error_sender, _error_reciever) = crossbeam::unbounded();[m
[32m+[m
[32m+[m[32m    // start spawning the parser threads[m
[32m+[m[32m    let _pool = rayon::ThreadPoolBuilder::new()[m
[32m+[m[32m        .num_threads(args.flags.thread_count)[m
[32m+[m[32m        .build()[m
[32m+[m[32m        .expect("couldn't build thread pool");[m
[32m+[m[41m    [m
[32m+[m[32m    args.inputs.iter().for_each(|path| { path_map.push_path(path.clone()); } );[m
[32m+[m
[32m+[m
[32m+[m[32m    explore_paths(&mut path_map, &mut scope_map, error_sender);[m
[32m+[m
[32m+[m[32m    println!("going to iter and open files");[m
[32m+[m
[32m+[m[32m    path_map.handles_mut().par_iter_mut().for_each(|file| {[m
[32m+[m[32m        file.open();[m
[32m+[m[32m    });[m
[32m+[m
[32m+[m[32m    let mut outers = Vec::new();[m
[32m+[m
[32m+[m[32m    path_map[m
[32m+[m[32m        .handles()[m
[32m+[m[32m        .par_iter()[m
[32m+[m[32m        .zip(scope_map.handles().par_iter())[m
[32m+[m[32m        .map(|(file, scope)| {[m
[32m+[m[32m            if let Some(handle_ref) = file.as_ref() {[m
[32m+[m[32m                let r = parse_unit(handle_ref, &*scope.read().unwrap(), &args.flags);[m
[32m+[m[32m                r[m
[32m+[m[32m            } else {[m
[32m+[m[32m                Ok(OuterScope::new(NodeInfo::Builtin, Vec::new()))[m
[32m+[m[32m            }[m
[32m+[m[32m        })[m
[32m+[m[32m        .collect_into_vec(&mut outers);[m
[32m+[m
[32m+[m[32m    outers[m
[32m+[m[32m        .par_iter()[m
[32m+[m[32m        .zip(path_map.handles().par_iter())[m
[32m+[m[32m        .zip(scope_map.handles_mut().par_iter_mut())[m
[32m+[m[32m        .for_each(|((outer, handle), scope_context)| {[m
[32m+[m[32m            if handle.path().is_file() {[m
[32m+[m[32m                if let Ok(root) = outer.as_ref() {[m
[32m+[m[32m                    println!("root was ok");[m
[32m+[m[32m                    let mut scope_guard = scope_context.write().unwrap();[m
[32m+[m[32m                    scope_guard.on_root(scope_context.clone(), root);[m
[32m+[m[32m                } else {[m
[32m+[m[32m                    println!("root was not ok");[m
[32m+[m[32m                }[m
[32m+[m[32m            } else {[m
[32m+[m[32m                println!("handle was not file");[m
[32m+[m[32m            }[m
[32m+[m[32m        });[m
[32m+[m
[32m+[m[32m    /*println!("root scopes:");[m
[32m+[m[32m    for s in scope_map.handles() {[m
[32m+[m[32m        let scope = s.read().unwrap();[m
[32m+[m[32m        println!("{}", scope);[m
[32m+[m[32m    }*/[m
[32m+[m[32m    println!("context tree:");[m
[32m+[m[32m    println!("{}", scope_map.global().unwrap().read().unwrap());[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mstruct ArgResult {[m
[32m+[m[32m    flags: CFlags,[m
[32m+[m[32m    inputs: HashSet<PathBuf>,[m
[32m+[m[32m    outputs: HashSet<PathBuf>,[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32mfn parse_args(args: &[&str]) -> Result<ArgResult, &'static str> {[m
     enum State {[m
         ExpectInput,[m
         ExpectOutput,[m
[36m@@ -88,6 +162,7 @@[m [mpub fn launch(args: &[&str]) {[m
         _ExpectErrorFlags,[m
     }[m
 [m
[32m+[m
     let mut state = State::ExpectInput;[m
 [m
     let mut cflags = CFlags {[m
[36m@@ -145,78 +220,9 @@[m [mpub fn launch(args: &[&str]) {[m
         println!("input file with path '{:#?}'", p);[m
     }[m
 [m
[31m-    let mut path_map = PathIdMap::new();[m
[31m-    let mut scope_map = ScopeIdMap::new();[m
[31m-[m
[31m-    let (error_sender, _error_reciever) = crossbeam::unbounded();[m
[31m-[m
[31m-    //static_assertions::assert_impl_all!(&mut [std::option::Option<crate::helper::FileHandle<'_>>]: rayon::iter::IntoParallelIterator);[m
[31m-    //static_assertions::assert_impl_all!(for<'data> &'data mut [Option<FileHandle<'data>>]: rayon::iter::IntoParallelIterator);[m
[31m-    //static_assertions::assert_impl_all!(FileHandle<'_>: Send);[m
[31m-[m
[31m-    // start spawning the parser threads[m
[31m-    let _pool = rayon::ThreadPoolBuilder::new()[m
[31m-        .num_threads(cflags.thread_count)[m
[31m-        .build()[m
[31m-        .expect("couldn't build thread pool");[m
[31m-[m
[31m-    for path in inputs.iter() {[m
[31m-        //println!("pushing path {:?}", path);[m
[31m-        path_map.push_path(path.clone());[m
[31m-    }[m
[31m-[m
[31m-    explore_paths(&mut path_map, &mut scope_map, error_sender);[m
[31m-[m
[31m-    println!("going to iter and open files");[m
[31m-[m
[31m-    path_map.handles_mut().par_iter_mut().for_each(|file| {[m
[31m-        file.open();[m
[31m-    });[m
[31m-[m
[31m-    let mut outers = Vec::new();[m
[31m-[m
[31m-    path_map[m
[31m-        .handles()[m
[31m-        .par_iter()[m
[31m-        .zip(scope_map.handles().par_iter())[m
[31m-        .map(|(file, scope)| {[m
[31m-            //println!("parsing a file");[m
[31m-            if let Some(handle_ref) = file.as_ref() {[m
[31m-                let r = parse_unit(handle_ref, &*scope.read().unwrap(), &cflags);[m
[31m-                r[m
[31m-            } else {[m
[31m-                Ok(OuterScope::new(NodeInfo::Builtin, Vec::new()))[m
[31m-                //println!("offending file is {:?}", file.path());[m
[31m-                //panic!("couldn't read from an opened file");[m
[31m-            }[m
[31m-        })[m
[31m-        .collect_into_vec(&mut outers);[m
[31m-[m
[31m-    println!("lens: {} {} {}", outers.len(), path_map.handles().len(), scope_map.handles().len());[m
[31m-[m
[31m-    outers[m
[31m-        .par_iter()[m
[31m-        .zip(path_map.handles().par_iter())[m
[31m-        .zip(scope_map.handles_mut().par_iter_mut())[m
[31m-        .for_each(|((outer, handle), scope_context)| {[m
[31m-            if handle.path().is_file() {[m
[31m-                if let Ok(root) = outer.as_ref() {[m
[31m-                    println!("root was ok");[m
[31m-                    let mut scope_guard = scope_context.write().unwrap();[m
[31m-                    scope_guard.on_root(scope_context.clone(), root);[m
[31m-                } else {[m
[31m-                    println!("root was not ok");[m
[31m-                }[m
[31m-            } else {[m
[31m-                println!("handle was not file");[m
[31m-            }[m
[31m-        });[m
[31m-[m
[31m-    println!("root scopes:");[m
[31m-    for s in scope_map.handles() {[m
[31m-        let scope = s.read().unwrap();[m
[31m-        println!("{}", scope);[m
[31m-    }[m
[32m+[m[32m    Ok(ArgResult {[m
[32m+[m[32m        inputs, outputs, flags: cflags,[m
[32m+[m[32m    })[m
 }[m
 [m
 pub fn explore_paths<'input, 'context>([m
[1mdiff --git a/src/mid_repr/outer.rs b/src/mid_repr/outer.rs[m
[1mindex 210e125..d0624eb 100644[m
[1m--- a/src/mid_repr/outer.rs[m
[1m+++ b/src/mid_repr/outer.rs[m
[36m@@ -63,6 +63,8 @@[m [mimpl<'input> AstNode<'input> for ScopeContext<'input> {[m
         } else {[m
             writeln!(f, "{}None", indent(depth+2)).unwrap();[m
         }*/[m
[32m+[m[32m        writeln!(f, "{}From:", indent(depth +1)).unwrap();[m
[32m+[m[32m        self.from.as_ref().map(|from_lock| from_lock.read().unwrap().display(f, depth + 2));[m
         writeln!(f, "{}Inner contexts:", indent(depth + 1)).unwrap();[m
         self.inner_contexts[m
             .clone()[m
