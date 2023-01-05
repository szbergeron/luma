use once_cell::{race::OnceBox, sync::OnceCell};
use tokio::sync::watch;
use async_recursion::async_recursion;

use super::node::{TypeSchemaID, VarID};

struct Block {}

/// An Allocation is a backing for something that
/// we want to resolve the type of. It has usages
/// and it has sources. Sources must be assign-compatible
/// into it, so those
struct Allocation {}

type Receiver = watch::Receiver<Option<Result<Resolution, TypeError>>>;

pub struct ExpressionID(usize);

/// An expression waits on its direct components to be available
struct Expression {
    //requires: Vec<ExpressionID>,
    notify: OnceCell<Receiver>,

    requires: Vec<VarID>
    //notify: watch::Receiver<Result<TypeSchemaID, TypeError>>,
}

#[derive(Clone, Copy)]
pub struct ConstValue {}

#[derive(Clone, Copy)]
pub struct Resolution {
    expression_type: TypeSchemaID,
    expression_value: Option<ConstValue>,
}

#[derive(Clone)]
pub struct TypeError {
    components: Vec<usize>,
    complaint: String,
}

impl Expression {
    /// Internally concurrently awaits a future on the resolved
    /// type of this expression
    #[async_recursion]
    pub async fn resolve(&self) -> Result<Resolution, TypeError> {
        let mut mine = None;

        let r = self.notify.get_or_init(|| {
            let (s, r) = watch::channel(None);

            mine = Some((s, r.clone()));

            r
        });

        let mut receiver = if let Some((ms, mr)) = mine && mr.same_channel(r) {
            // This unsafe is sound since we never
            // dealloc the expression slice until 
            // all computations are complete
            let sptr: &'static Self = unsafe { std::mem::transmute(&self) };

            // we own the computation :) so now we need to spawn the work
            tokio::spawn(async move {
                //let self = std::mem::transmute(self);

                let res = sptr.solve().await;

                // steal the sender
                ms.send(Some(res));
            });

            mr

        } else {
            r.clone() // only need to clone in this case out of the reference,
                      // since otherwise we can use "our" receiver that
                      // we already made
        };

        loop {
            receiver.changed().await;

            if let Some(r) = receiver.borrow().clone() {
                // a value was put
                break r; // computation finished, maybe with error
            }
        }
    }

    async fn solve(&self) -> Result<Resolution, TypeError> {
        // TODO: do the computation for this expression, await its dependants

        // first wait on our dependants
        for e in self.requires.iter() {
            // await solve
            let exp: &Expression = get_exp(*e);

            let part = exp.resolve().await;
        }

        // then check if we have the needed information
        // to solve directly, or if we need to ask the oracle
        //let sref: &Self = unsafe { sptr.as_ref() };
        todo!()
    }
}

pub fn get_exp(s: VarID) -> &'static Expression {
    todo!()
}

impl Block {
    pub async fn solve() {
        // figure out semantic dependencies, submit fact requests for expressions,
        // block on waiting for next info dump

        //
    }
}
