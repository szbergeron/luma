

    /*
    pub async fn emit_broadcasts(&mut self) {
        let self_as_dest = self.as_dest();

        for (fid, fc) in self.dynamic_field_contexts.iter_mut() {
            let directs = fc.resolutions.iter();

            let mut counts: HashMap<ResolvedType, fixed::types::I16F16> = HashMap::new();

            for (usage, resolution) in directs {
                *counts.entry(resolution.clone()).or_insert(0.0.to_fixed()) +=
                    (1.0).to_fixed::<fixed::types::I16F16>();
            }

            let mut weights = Vec::new();

            let len = counts.len() as f32;

            for (ty, count) in counts.into_iter() {
                // this can't be a div by zero since len must have been > 0 to get into this loop
                weights.push((count / len.to_fixed::<fixed::types::I16F16>(), ty));
            }

            if weights.len() > 1 {
                todo!("emit a type error, we have conflicting assignments");
            }

            if !weights.is_empty() {
                // we get to choose one and do a broadcast
                //weights.sort_unstable_by_key(|(a, b)| *a);
                let (weight, ty) = weights
                    .iter()
                    .max_by_key(|(a, b)| *a)
                    .expect("wasn't empty, but no max?")
                    .clone();

                tracing::info!("node {:?} commits to type {ty:?}", self.for_ctx);

                let announce = Arc::new(AnnounceCommit {
                    from_source: *fid,
                    commits_to: ty,
                    weights,
                });

                for (dest, target_field) in
                    self.notify_when_resolved.remove(fid).unwrap_or(Vec::new())
                {
                    let m = Memo::AnnounceCommit {
                        original: announce.clone(),
                        for_field: target_field,
                    };

                    let m = Message {
                        to: dest,
                        from: self_as_dest,
                        send_reply_to: Destination::nil(),
                        conversation: uuid::Uuid::new_v4(),
                        content: Content::Transponster(m),
                    };

                    self.earpiece.send(m);
                }

                // now the last one
            }
        }
    }
    */

    /*pub async fn add_usage(&mut self, field: FieldID, value_type: TypeVar) {
        assert!(
            field.on == self.for_ctx,
            "can only add a usage where the field is on self, otherwise it's nonsense"
        );

        match value_type.current {
            TypeType::Resolved(box rt) => {
                //self.add_direct(field, rt).await;
                let fc = self.dynamic_field_contexts.entry(field).or_insert(FieldContext::new_for_field(field));

                fc.resolutions
            }
            TypeType::Symbolic(box sy) => {
                // we need to add an indirect here and set a waiter on it
            }
            TypeType::Refer(_) => {
                panic!("transponster was given a refer but should have been given a root")
            }
            TypeType::Unknown() => {
                panic!("what")
            }
        }
    }*/

    /*
    pub fn is_regular_field(&self, field: IStr) -> bool {
        self.regular_fields.contains_key(&field)
    }

    /// If the instantiation didn't
    /// allow us to resolve the type of the field directly,
    /// then we return the determining TypeID
    ///
    /// We may wait to resolve until we have committed to a type
    /// in the case that we will *eventually* have enough information
    /// to make that determination
    pub async fn wait_solve_field(
        &mut self,
        field: IStr,
        on: Arc<Instantiation>,
    ) -> PortableTypeVar {
        if let Some(v) = self.regular_fields.get(&field) {
            // turn the STR into a lowered form according to the types in the Instantiation
            match v.resolve().unwrap().inner.clone() {
                crate::cst::SyntacticTypeReferenceInner::Single { name } => {
                    if let [one] = name.scope.as_slice() && self.generics.contains(one) {
                        //PortableTypeVar::UnPortable(on.generics[one].clone())
                        on.generics[one].clone()
                    } else {
                        let nr = NameResolver {
                            name,
                            based_in: self.for_ctx.resolve().parent.unwrap(),
                            reply_to: self.for_ctx,
                            service: Service::Oracle()
                        };

                        let resolved = nr
                            .using_context(&self.conversations)
                            .await;
                        let resolved = resolved.expect("need to handle unresolved names");

                        // no generics since this is just a single (the other ones get more
                        // complicated)
                        PortableTypeVar::Instantiation(Arc::new(Instantiation {
                            generics: HashMap::new(),
                            of_base: resolved,
                        }))
                    }
                }
                _ => todo!("other kinds of STR"),
            }
        } else {
            // could be a dynamic field, so this is a new usage
            unsafe {
                let f = self
                    .dynamic_fields
                    .entry(field)
                    .or_insert(UnsafeAsyncCompletable::new());

                f.clone().wait().await
            }
        }
    }*/

    /*
    pub async fn typeof_regular_field(&self, field: IStr, within: &mut Quark) -> Option<TypeID> {
        if let InstanceOf::Type(InstanceOfType {
            regular_fields,
            methods: regular_methods,
            from,
        }) = &self.of
        {
            match &regular_fields.get(&field) {
                None => None,
                Some(tid) => Some(tid.clone().extract().await),
            }
        } else {
            todo!("user tried to get a field on a function type")
        }
    }

    /// If we already have a type for the DynField, then return it here
    pub fn typeof_dynamic_field(&self, field: IStr) -> Option<ResolvedType> {
        todo!()
    }*/

    /*

    /// the direction here says, if Load then "the field is loaded from into something of TID
    /// <with_tid>, and the inverse if direction is Store
    pub fn use_field(
        &self,
        field: IStr,
        with_tid: TypeID,
        direction: UsageDirection,
    ) -> (UsageHandle, Vec<Unify>) {
        let mut unify = Vec::new();

        match &self.of {
            InstanceOf::Type(t) => {
                if let Some(&field_type) = t.regular_fields.get(&field) {
                    todo!("neat");
                    let u = match direction {
                        UsageDirection::Load() => Unify {
                            from: field_type,
                            into: with_tid,
                        },
                        UsageDirection::Store() => Unify {
                            from: with_tid,
                            into: field_type,
                        },
                    };

                    unify.push(u);

                    (UsageHandle(uuid::Uuid::new_v4()), unify)
                //} else if let Some(pair) = t.m
                } else {
                    // this is a dynamic field
                    todo!("dynamic fields")
                }
            }
            InstanceOf::Func(f) => todo!("unify function calls"),
            InstanceOf::Generic(g) => {
                todo!("what now?")
            }
            InstanceOf::Unknown() => todo!("rip"),
        }
    }

    /// allows us to add a direct
    /// if the field was already resolved, this returns a TypeError describing the
    /// mismatch
    pub fn resolve_usage(
        &self,
        usage: UsageHandle,
        direction: UsageDirection,
        to_type: ResolvedType,
    ) -> Result<(), TypeError> {
        todo!()
    }

    /*pub fn use_field(
        &self,
        name: IStr,
    ) -> UnsafeAsyncCompletableFuture<T> {
    }*/

*/


    fn field_handle_mut(
        fields: &mut HashMap<FieldID, FieldContext>,
        field: FieldID,
    ) -> &mut FieldContext {
        fields.entry(field.clone()).or_insert(FieldContext {
            for_field: field,
            received_broadcasts: HashSet::new(),
            usages: HashSet::new(),
            resolutions: HashMap::new(),
        })
    }

    fn field_handle(&mut self, field: FieldID) -> &FieldContext {
        self.dynamic_field_contexts
            .entry(field.clone())
            .or_insert(FieldContext {
                for_field: field,
                received_broadcasts: HashSet::new(),
                usages: HashSet::new(),
                resolutions: HashMap::new(),
            })
    }

    pub async fn resolve_usage(
        &mut self,
        field: FieldID,
        usage: UsageHandle,
        value_type: ResolvedType,
    ) {
        assert!(
            field.on == self.for_ctx,
            "if it isn't on self, then it isn't a direct"
        );

        let field_context = Self::field_handle_mut(&mut self.dynamic_field_contexts, field);

        assert!(
            field_context.usages.contains(&usage),
            "we don't have a matching usage for this"
        );

        let prior = field_context.resolutions.insert(usage, value_type);

        assert!(
            prior.is_none(),
            "they already resolved this usage, why are they doing it again"
        );
    }

    pub fn send_announce_to(
        from: Destination,
        ep: &mut Earpiece,
        ac: Arc<AnnounceCommit>,
        to: FieldID,
    ) {
        ep.send(Message {
            to: Destination::transponster(to.on),
            from,
            send_reply_to: from,
            conversation: uuid::Uuid::new_v4(),
            content: Content::Transponster(Memo::AnnounceCommit {
                original: ac,
                for_field: to,
            }),
        })
    }

    /// Takes a field on self and says
    pub async fn add_indirect(
        &mut self,
        on_self: FieldID,
        on_other: FieldID,
        _direction: AssignmentDirection,
    ) {
    }


#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub struct UsageHandle(uuid::Uuid);

/// We can potentially extend this into supporting HKTs
/// by allowing generics to be provided on an opaque TypeID
///
/// though, that will make actually doing the stepped
/// dyn field solve a lottttt harder
#[derive(Debug, Clone)]
pub enum PortableTypeVar {
    UnPortable(TypeID),
    /// a free type that should be matched to a new TypeID within the solver system
    Free(),
    Instantiation(Arc<Instantiation>),
}

#[derive(Debug, Clone)]
pub struct Instantiation {
    /// eventually will make this support constraints as alternatives on it
    /// instead of just either a TypeID that was passed through or fully
    /// unconstrained
    generics: HashMap<IStr, PortableTypeVar>,

    of_base: CtxID,
}

struct FieldContext {
    for_field: FieldID,

    //// map of <Origin, Recipient>
    //forwarded_broadcasts: HashMap<FieldID, HashSet<FieldID>>,
    //// Map<Origin Field, Map<Local Field, (Received Count, Decided Type, Decision Weights)>>
    //received_broadcasts: HashMap<FieldID, HashMap<FieldID, (usize, ResolvedType, Vec<(f32, ResolvedType)>)>>,
    received_broadcasts: HashSet<Arc<AnnounceCommit>>,

    usages: HashSet<UsageHandle>,

    resolutions: HashMap<UsageHandle, ResolvedType>,
}

impl FieldContext {
    pub fn new_for_field(for_field: FieldID) -> Self {
        Self {
            for_field,
            received_broadcasts: HashSet::new(),
            usages: HashSet::new(),
            resolutions: HashMap::new(),
        }
    }

    /// After any given pass, this takes what we know
    /// about the usage of this field and tries to
    /// put a number on how sure we are that
    /// we are the type we think, and thus how good a
    /// candidate we are for committing
    ///
    /// Any number over 0.95 should imply we are a candidate
    /// for immediate commit, without trying to do an election
    pub fn calculate_certainty(&self) -> fixed::types::I16F16 {
        // simply take how many direct usages we have and compare that
        // to how many unknowns we have, for now
        //
        // we may want to include an additional biasing operation later
        // where we get more confident with more directs even
        // with a lower ratio
        let usage_count = self.usages.len() as f64;
        let resolution_count = self.resolutions.len() as f64;

        let ratio = if usage_count > 0.0 {
            resolution_count / usage_count
        } else {
            // if we have no usages, we have no certainty and no type we could possibly be
            0.0
        };

        ratio.to_fixed()
    }
}
