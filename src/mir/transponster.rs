use std::{collections::{HashMap, HashSet}, sync::Arc};

use tracing::warn;

use crate::{
    ast::{
        executor::Executor, resolver2::SymbolResolver, tree::CtxID, types::StructuralDataDefinition,
    },
    compile::per_module::{Earpiece, Message, Destination, Content},
    helper::interner::IStr,
};

use super::{
    expressions::AssignmentDirection,
    quark::{ResolvedType, TypeType, TypeVar},
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

    //resolutions: HashMap<FieldID, >,

    //assignments: HashMap<FieldID, Vec<TypeVar>>,
    dynamic_field_contexts: HashMap<FieldID, FieldContext>,

    /// If it's in here, then we've committed to a type for that
    /// variant/field and it can be used for inference
    dynamic_fields: HashMap<FieldID, FieldType>,

    notify_when_resolved: HashMap<FieldID, Vec<Destination>>,

    regular_fields: HashMap<IStr, FieldType>,
}

struct FieldContext {
    for_field: FieldID,

    /// map of <Origin, Recipient>
    //forwarded_broadcasts: HashMap<FieldID, HashSet<FieldID>>,
    /// Map<Origin Field, Map<Local Field, (Received Count, Decided Type, Decision Weights)>>
    //received_broadcasts: HashMap<FieldID, HashMap<FieldID, (usize, ResolvedType, Vec<(f32, ResolvedType)>)>>,

    received_broadcasts: HashSet<Arc<AnnounceCommit>>,

    direct_assignments: Vec<ResolvedType>,

    assigns_into: Vec<FieldID>,
}

pub enum FieldType {
    Known(ResolvedType),
    Generic(IStr),
}

#[derive(Debug, Clone)]
pub enum Memo {
    /// AnnounceCommit is used when a source says they have
    /// directs and have committed to a given type
    AnnounceCommit { original: Arc<AnnounceCommit>, for_field: FieldID},

    /// If you are sent this, it means you asked a while ago
    /// about a field and we didn't know what type it was
    ///
    /// We now know the type, or know that it can not be resolved, so
    /// you can ask us again for more information
    CheckAgain { field: FieldID },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnnounceCommit {
        from_source: FieldID,
        commits_to: ResolvedType,
        weights: Vec<(fixed::types::U10F22, ResolvedType)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FieldID {
    name: IStr,
    on: CtxID,

    /// This is a field on a struct parameterized by
    /// the given typevar. If the struct this is on
    /// is not generic, this is empty
    parameterized_by: Vec<ResolvedType>,
}

impl Transponster {
    pub fn as_dest(&self) -> Destination {
        Destination::transponster(self.for_ctx)
    }

    pub fn for_node(node_id: CtxID, earpiece: Earpiece) -> Self {
        let regular_fields = HashMap::new();

        Self {
            for_ctx: node_id,
            earpiece,

            dynamic_field_contexts: HashMap::new(),
            dynamic_fields: HashMap::new(),

            notify_when_resolved: HashMap::new(),
            
            regular_fields,
        }
    }

    pub async fn entry(mut self, executor: &'static Executor, sd: &mut StructuralDataDefinition) {
        let parent_id = self.for_ctx.resolve().parent.unwrap();

        for field in sd.fields.iter_mut() {
            let tres = SymbolResolver {
                node_id: self.for_ctx,
                earpiece: &mut self.earpiece,
                for_service: crate::compile::per_module::Service::Oracle(),
            }
            .resolve(field.has_type.as_mut().unwrap())
            .await;

            match field.has_type.as_ref().expect("no type?") {
                crate::cst::TypeReference::Syntactic(_) => todo!(),
                crate::cst::TypeReference::Abstract(box a, _) => {
                    todo!()
                }
            }

            warn!("transponster resolved a field type");
        }
    }

    fn field_handle_mut(fields: &mut HashMap<FieldID, FieldContext>, field: FieldID) -> &mut FieldContext {
        fields.entry(field.clone()).or_insert(FieldContext {
            for_field: field,
            received_broadcasts: HashSet::new(),
            direct_assignments: Vec::new(),
            assigns_into: Vec::new(),
        })
    }

    fn field_handle(&mut self, field: FieldID) -> &FieldContext {
        self.dynamic_field_contexts.entry(field.clone()).or_insert(FieldContext {
            for_field: field,
            received_broadcasts: HashSet::new(),
            direct_assignments: Vec::new(),
            assigns_into: Vec::new(),
        })
    }

    pub async fn wait_for(&mut self, field: FieldID) -> Result<TypeVar, !> {
        todo!()
    }

    pub async fn add_direct(&mut self, field: FieldID, value_type: ResolvedType) {
        assert!(
            field.on == self.for_ctx,
            "if it isn't on self, then it isn't a direct"
        );

        let field_context = Self::field_handle_mut(&mut self.dynamic_field_contexts, field);

        field_context.direct_assignments.push(value_type);
    }

    pub fn receive_broadcast(&mut self, ac: Arc<AnnounceCommit>, for_field: FieldID) {
        let sd = self.as_dest();
        let fc = Self::field_handle_mut(&mut self.dynamic_field_contexts, for_field);

        let already_received = fc.received_broadcasts.insert(ac.clone());

        if !already_received {
            // we then forward it along
            for f in fc.assigns_into.iter() {
                Self::send_announce_to(sd, &mut self.earpiece, ac.clone(), f.clone());
            }
        }
    }

    pub fn send_announce_to(from: Destination, ep: &mut Earpiece, ac: Arc<AnnounceCommit>, to: FieldID) {
        ep.send(Message {
            to: Destination::transponster(to.on),
            from,
            send_reply_to: from,
            conversation: uuid::Uuid::new_v4(),
            content: Content::Transponster(Memo::AnnounceCommit { original: ac, for_field: to }),
        })
    }

    pub async fn ask_type_of(&mut self, field: FieldID, notify_later: Destination) -> Result<FieldType, ()> {
        todo!()
    }

    pub async fn emit_broadcasts(&mut self) {
        for (fid, fc) in self.dynamic_field_contexts.iter_mut() {
            let directs = fc.direct_assignments.iter();

            let mut counts = HashMap::new();

            for direct in directs {
                *counts.entry(direct.clone()).or_insert(0.0f32) += 1.0;
            }

            let mut weights = Vec::new();

            let len = counts.len() as f32;

            for (ty, count) in counts.into_iter() {
                // this can't be a div by zero since len must have been > 0 to get into this loop
                weights.push((count / len, ty));
            }

            if !weights.is_empty() {
                // we get to choose one and do a broadcast
            }
        }
    }

    pub async fn add_usage(&mut self, field: FieldID, value_type: TypeVar) {
        assert!(
            field.on == self.for_ctx,
            "can only add a usage where the field is on self, otherwise it's nonsense"
        );

        match value_type.current {
            TypeType::Resolved(box rt) => {
                self.add_direct(field, rt).await;
            }
            TypeType::Symbolic(sy) => {
                // we need to add an indirect here and set a waiter on it
            }
            TypeType::Refer(_) => {
                panic!("transponster was given a refer but should have been given a root")
            }
            TypeType::Unknown() => {
                panic!("what")
            }
        }
    }

    /// Takes a field on self and says
    pub async fn add_indirect(
        &mut self,
        on_self: FieldID,
        on_other: FieldID,
        _direction: AssignmentDirection,
    ) {
    }

    /// Called every time the quark phase finishes, so for each field we have
    /// that isn't yet sealed that we have new directs on, we can seal them
    /// and commit to a type
    pub async fn end_quark_phase(&mut self) {}

    /// If a certainty for the field *can* be computed,
    /// then a set of the typevars that it could be are returned, each
    /// paired with how likely that var is to be the case
    pub async fn compute_certainty(&mut self, field: FieldID) -> Option<Vec<(TypeVar, f64)>> {
        todo!()
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
