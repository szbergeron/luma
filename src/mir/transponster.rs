use tracing::warn;

use crate::{
    ast::{
        executor::Executor, resolver2::TypeResolver, tree::CtxID, types::StructuralDataDefinition,
    },
    compile::per_module::Earpiece,
};

/// Yes, it's a reference, and no, it's not a good one
///
/// This handles the work of doing inference on dynamic fields
/// given a type. It uses a stepped model, where all assignments
/// to a field are grouped based on staged visibility.
///
/// Quark on every node first runs a round, and tells
/// Transponster what assignments were made to any fields
/// on it within the initial round where the type of the
/// source expression could be fully inferred.
/// Quark also tells a transponster about every time
/// a field is assigned into it of unknown type,
/// and if a dynamic field is read it tells the
/// source transponster that the field was read
///
/// These pieces of information build a value flow
/// graph within the transponsters, and allow
/// for selecting root fields where the most
/// information is known about their type,
/// and where the fewest unknowns are assigned into them.
///
/// This is a potentially "unstable" (though deterministic
/// for any specific textual input) metric, so care must be
/// taken to craft not specifically the most accurate metric
/// (to provide as comprehensive inference as we could) but
/// rather to provide the most predictable and stable metric,
/// where we may group types and pull roots concurrently
/// even if there is a specific root that could provide
/// information for dependents
pub struct Transponster {
    for_ctx: CtxID,
    earpiece: Earpiece,
}

impl Transponster {
    pub fn for_node(node_id: CtxID, earpiece: Earpiece) -> Self {
        Self {
            for_ctx: node_id,
            earpiece,
        }
    }

    pub async fn entry(mut self, executor: &'static Executor, sd: &mut StructuralDataDefinition) {
        let parent_id = self.for_ctx.resolve().parent.unwrap();

        for field in sd.fields.iter_mut() {
            let tres = TypeResolver {
                node_id: self.for_ctx,
                earpiece: &mut self.earpiece,
                for_service: crate::compile::per_module::Service::Oracle(),
            }
            .resolve(field.has_type.as_mut().unwrap()).await;

            warn!("transponster resolved a field type");
        }
    }

    pub async fn thread(self, executor: &'static Executor) {
        warn!("starting transponster");

        let node = &self.for_ctx.resolve().inner;

        match node {
            crate::ast::tree::NodeUnion::Type(t) => {
                self.entry(executor, &mut t.lock().unwrap()).await;
            }
            _other => {
                warn!("Transponster shuts down since this node type wasn't a Type")
            }
        }
    }
}
