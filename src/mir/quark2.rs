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

pub enum OperationResult {
    /// If this block could not be resolved correctly,
    /// then Error() is returned. Error() substitutes for every other
    /// variant, and TODO: figure out if we can still compile
    /// even with type errors and simply panic
    Error(),

    /// This block yields no result, and
    /// purely produces side effects
    Empty(),

    /// This block yields a unified result
    /// in a single value (not destructured, can still be composite)
    Single(IStr),
}

pub struct MoveOperation {
    from: AllocationReference,
    into: AllocationReference,
}

pub struct AssignOperation {
    /// Need to change this into pattern syntax at
    /// some point for destructuring,
    /// for now just make it a variable or reference
    into: OperationReference,

    value: OperationReference,
}

/// Does nothing, but falls through
/// to the next operation just like
/// other (non-return) operations
///
/// Used for lowering loops
pub struct NoOperation {}

/// A neverop should never be reachable from
/// anywhere in the code (should not be a connected
/// leaf in any CFG).
pub struct NeverOperation {}

/// Any node connected to an ExitOperation
/// must provide a result value,
/// and that result value forms an exit from the
/// CFG. This node appears
/// at the end of functions only, and forms
/// the basis for implicit return
pub struct ExitOperation {}

pub struct CallOperation {
    base: OperationReference,

    named: IStr,

    type_args: Vec<SymbolicType>,

    result_type: Option<SymbolicType>,

    resolved_target: Option<CtxID>,
}

pub struct ReturnOperation {
    value: SymbolicType,
}

pub struct JumpOperation {
    to: OperationReference,
}

pub struct ModifyOperation {
    /// The target of a Modify,
    /// the value in `from` is added as an implementation
    /// to this object
    into: OperationReference,

    /// Must yield an expression of Implemetation
    /// type
    from: OperationReference,
}

pub struct ImplementOperation {}

pub struct GuardOperation {
    /// The guard target
    check: OperationReference,

    /// GuardOperation checks that the value in `check` is an `is`
    is: SymbolicType,

    then: OperationReference,

    otherwise: Option<OperationReference>,

    /// Not yet useful, will allow statically
    /// ensuring that a type propagates for a given usage for any possible path
    ///
    /// Basically allows for a guard inside an `if true` or on the basic path
    /// to force itself to pass, so if we're a closure called
    /// inside of a function that isn't offering generics but we can know that
    /// we're passing in a given type, that intermediate function can be monomorphised to
    /// pass additional information and not require a recheck of the type
    require_resolvable: bool,
}

pub struct BranchOperation {
    /// Use `eval` as the descriminant for the branch,
    /// it should either produce a bool (an i1) or
    /// a larger integer type (that may have more data from guards)
    /// that allows a "switch"
    eval: OperationReference,

    /// Take the result of `eval` as an integral
    /// type aliased `n` and branch to the `n`th
    /// entry in `targets` before converging at the end of
    /// this operation
    ///
    /// All entries in `targets` should
    /// be possible to narrow to a single type,
    /// with additional checks possible to warn
    /// about very loose divergences
    targets: SmallVec<[OperationReference; 2]>,
}

/// A name is optional here as
/// binding can occur during destructuring
/// but is different from
/// a destructure operation.
///
/// Think if someone does `let (a, b): (A, B);`,
/// the bind is done to an allocation of type (A, B)
/// even though that bind itself is unnamed
pub struct BindOperation {
    allocation_type: SymbolicType,

    named: Option<IStr>,

    allocation: AllocationReference,
    //level: usize,
}

/// Any bindings at a level equal to or greater than `level`
/// expire here
pub struct UnbindOperation {
    level: usize,
}

/*enum Type {
    Pointer(Box<Type>),
    Reference(Box<Type>),
    Generic(GenericConstraint),
    Value(SymbolicType),
    Dynamic(SymbolicType),
}*/

enum Fact {
    /// The index of the generic, as well as the type that we have info on it for
    Generic(usize, SymbolicType),

    /// A possible base type for this
    Base(SymbolicBase),
}

