use async_recursion::async_recursion;
use dashmap::DashMap;
use futures::future::{join, join_all};

use crate::{
    cst,
    helper::{interner::IStr, VecOps},
    lex::ParseResultError,
};

use super::{
    file_tree::{FileHandle, FileRegistry},
    parse_source_file,
    preparse_tree::PreParseTreeNode,
    CFlags,
};

#[derive(Debug)]
pub struct ParsedFile<'r> {
    file: FileHandle<'r>,

    value: Option<cst::OuterScope>,

    errors: Vec<ParseResultError>,
}

impl<'r> ParsedFile<'r> {
    pub async fn from_file(
        file: FileHandle<'r>,
        scope: Vec<IStr>,
        cflags: &CFlags,
    ) -> ParsedFile<'r> {
        let res = parse_source_file(file, scope, cflags);

        /*let (value, errors) = match res {
            Ok(v) => (Some(v), Vec::new()),
            Err(e) => (None, vec![e]),
        };*/

        let (value, errors) = res;
        let errors = errors.to_vec();

        println!("Creates parsedfile");

        ParsedFile {
            file,
            value,
            errors,
        }
    }
}

#[derive(Debug)]
pub struct ParseTreeNode<'r> {
    files: &'r FileRegistry,

    parsed: Vec<ParsedFile<'r>>,

    children: DashMap<IStr, ParseTreeNode<'r>>,
}

struct SendPtr<V>(V);

unsafe impl<T> Send for SendPtr<T> {}

impl<'r> ParseTreeNode<'r> {
    pub async fn from_preparse<'s>(
        node: PreParseTreeNode<'s>,
        scope: Vec<IStr>,
        cflags: &'s CFlags,
    ) -> ParseTreeNode<'s> {
        // I know what this looks like, trust me...it's ok...
        unsafe {
            std::mem::transmute(
                Self::from_preparse_inner(
                    std::mem::transmute(node),
                    scope,
                    std::mem::transmute(cflags),
                )
                .await,
            )
        }
    }

    #[async_recursion]
    async unsafe fn from_preparse_inner(
        node: PreParseTreeNode<'static>,
        scope: Vec<IStr>,
        cflags: &'static CFlags,
    ) -> ParseTreeNode<'static> {
        let PreParseTreeNode {
            files,
            native,
            children,
        } = node;

        let mut file_futures = Vec::new();

        //let files: std::sync::Mutex<Vec<ParsedFile>> = std::sync::Mutex::new(Vec::new());

        // start the file parse process
        for file in native.into_iter() {
            /*let borrowed: &'static std::sync::Mutex<Vec<ParseResultError>> = unsafe {
                let r = &errors;
                let r: &'static std::sync::Mutex<Vec<ParseResultError>> = std::mem::transmute(r);
                r
            };*/

            //let ptr = &files as *const std::sync::Mutex<Vec<ParsedFile>>;

            //let sptr = SendPtr(ptr);
            let scope = scope.clone();

            let h = tokio::spawn(async move {
                //let borrowed = unsafe { sptr.0.as_ref().unwrap() };
                //borrowed.lock().unwrap().push(todo!());

                ParsedFile::from_file(file, scope.to_owned(), cflags).await
            });

            //let _ = h.await;

            file_futures.push(h);
        }

        let mut child_futures = Vec::new();

        for (name, child) in children.into_iter() {
            // This is fine since these futures can not cancel
            // and are unconditionally joined before editing the function.
            // We return with a lifetime of 'r since we reuse the lifetime of the input
            let child: PreParseTreeNode<'static> = std::mem::transmute(child);

            let scope = scope.clone().appended(name);

            let h = tokio::spawn(async move {
                (
                    name,
                    ParseTreeNode::from_preparse(child, scope, cflags).await,
                )
            });

            child_futures.push(h);
        }

        let (parsed, children) = join(join_all(file_futures), join_all(child_futures)).await;

        let children = children.into_iter().map(|r| r.unwrap()).collect();
        let parsed = parsed
            .into_iter()
            .map(|obj| {
                obj.map_err(|e| {
                    println!("\n\nHandled a panic\n\n");
                    let e = e.into_panic();
                    std::panic::resume_unwind(e);
                })
                    .unwrap()
            })
            .collect();
        //let parsed = parsed.into_iter().map(|r| r.unwrap()).collect();

        //join_all(handles).await;

        //let files = files.into_inner().unwrap();
        println!("Returns PTN");

        ParseTreeNode {
            files,
            parsed,
            children,
        }
    }
}
