use std::collections::{HashMap, HashSet, VecDeque};

/// This is a start-from-scratch expression type solution engine, just like my other 6
/// start-from-scratch attempts at making a type solution engine except this one
/// will *definitely* work

enum Expression {
    BinaryOperator(),
    Identifier(),
    Literal(),
    DynamicAccess(),
    DynamicLiteral(),
    FieldAccess(),
    ArrayAccess(),
    Assignment(),

    /// Yes, a type reference is in fact an expression
    ///
    /// A type reference can be used as an ID for matching
    /// tags or can have the variadic Construct operation applied to it
    ///
    /// This all allows type aliases within variables, but only at compile time
    ///
    /// If variables of type Type cannot be completely resolved then an error
    /// should be emitted, as every compositionally unique
    /// Type expression has a distinct type, in other words
    /// (TypeA,) and (TypeB,) even though both are Type expressions
    /// in arity-1 tuples, have different types.
    ///
    /// All Type expressions are (and must be) const
    Type(),
}

struct Assignment {
    left: ExpressionID,
    right: ExpressionID,
}

struct FieldAccess {
}

struct DynamicAccess {
    base: ExpressionID,

    tag: ExpressionID,
}

struct DynamicLiteral {
}

struct Expressionable {
    operation: Expression,

    etype: Future<Type>,
}

struct NodeInstance {
    dynmem: HashMap<ConstValue, NodeInstanceID>,

    members: HashMap<IStr, Member>,

    instanceof: NodeID,
}

struct TypeInstance {
    chain: Vec<NodeInstanceID>,
}

enum Answer {
    Yes,
    No,
    Maybe,
}

enum Operation {
    Add(),
    Subtract(),
    
    Call(CallOp),

    DynIndex(),

    DynConstruct(DynConstructOp),

    Literal(LiteralOp),

    Variable(VariableOp),
}

struct CallOp {
    name: IStr,
}

struct DynConstructOp {
    field_names: Vec<IStr>,
    field_types: Vec<Option<TypeInstance>>,
}

// A value is basically an expression
struct Value {
    operation: Operation,

    derived_from: ExpressionID,

    value: Option<ConstValue>,

    is_a: Future<Type>,
}

struct Satisfier {
    requires: Vec<ValueID>,

    required_for: Vec<ValueID>,

    value_requirements_remaining: HashSet<ValueID>,
}

impl Satisfier {
    /// This satisfier is "complete", so notify all dependants that
    /// we're done
    pub fn dependants(&self) -> Vec<ValueID> {
        self.required_for.clone()
    }

    pub fn notify(&mut self, completed: ValueID) {
        if self.requires.contains(&completed) {
            self.value_requirements_remaining -= 1;
        }
    }

    pub fn is_satisfied(&self) -> bool {
        self.value_requirements_remaining == 0
    }
}

impl Value {
    pub fn is_const(&self) -> Answer {
        todo!()
    }
}

struct Solver {
    expressions: HashMap<ValueID, Value>,
}

impl Solver {
    pub fn solve(&mut self) {
        // first populate our list of leaves

        let waits = VecDeque::new();

        for (val_id, val) in self.expressions.iter() {
            if val.is_satisfied() {
                waits.push_back(val_id);
            }
        }

        while waits.len() > 0 {
        }

        for (val_id, val) in self.expressions.iter() {
            // if anything is unsatisfied then we weren't able to resolve, so that's an error
            if !val.is_satisfied() {
                // program error
            }
        }
    }
}


