
pub struct CompositeResolver {
    ongoing_composites: HashMap<ScopedName, Uuid>,

    /// Values are of the form (remaining_scope, full_scope)
    /// we only reply when either we fail to resolve a step
    /// or when remaining_scope is []
    next_symbol_for: HashMap<(CtxID, IStr), Vec<CompositeState>>,
}


#[derive(Clone, Debug)]
pub struct ConversationState {
    reply_to: Destination,
    conversation: Uuid,
    inner: ConversationStateInner,
}

#[derive(Clone, Debug)]
enum ConversationStateInner {
    Composite(CompositeState),
    Single(SingleState),
}

#[derive(Clone, Debug)]
struct SingleState {
    symbol: IStr,
    within: CtxID,
}

#[derive(Clone, Debug)]
struct CompositeState {
    full_scope: ScopedName,
    resolved_elements: usize,

    current_symbol: IStr,
    currently_within: CtxID,
    originally_within: CtxID,
}

impl CompositeResolver {
    pub fn handle_resolve(
        &mut self,
        symbol: IStr,
        within: CtxID,
        resolution: Option<SimpleResolution>,
    ) {
        match self.next_symbol_for.remove(&(within, symbol)) {
            Some(v) => {
                for sn in v {
                    self.resolve_with(sn, resolution.clone());
                }
            }
            None => {
                // do nothing here, since we didn't have any components that
                // were waiting for that symbol as a local composite
            }
        }
    }

    fn resolve_with(&mut self, current: CompositeState, resolution: Option<SimpleResolution>) {
        match resolution {
            Some(v) => {
                //
            }
            None => {}
        }
    }
}

/*#[derive(Clone, Debug)]
pub struct Conversation {
    id: Uuid,
    messages: Vec<Message>,
}*/

/*
pub struct SymbolResolver<'ep> {
    //pub tr: &'ep mut TypeReference,
    pub node_id: CtxID, // the scope we're resolving within
    pub earpiece: &'ep mut Earpiece,
    pub for_service: Service,
}

impl<'ep> SymbolResolver<'ep> {
    pub fn as_dest(&self) -> Destination {
        Destination {
            node: self.node_id,
            service: self.for_service,
        }
    }

    pub async fn resolve(mut self, tr: &mut OldTypeReference) {
        self.resolve_typeref(tr).await;
    }

    pub async fn resolve_typeref(&mut self, tb: &mut OldTypeReference) {
        let abstrakt: &mut AbstractTypeReference = match tb {
            OldTypeReference::Syntactic(s) => {
                let generic_args = self
                    .node_id
                    .resolve()
                    .generics
                    .iter()
                    .map(|(name, _tr)| *name)
                    .collect_vec();
                let abstrakt = s.to_abstract(generic_args.as_slice());

                *tb = OldTypeReference::Abstract(box abstrakt, *s);

                match tb {
                    OldTypeReference::Abstract(a, s) => &mut *a,
                    _ => unreachable!(),
                }
            }
            OldTypeReference::Abstract(a, s) => {
                panic!("this shouldn't have already been made abstract yet, that was our job!")
            }
        };

        let mut base = abstrakt.inner.write().unwrap();

        match &mut *base {
            crate::ast::types::TypeBase::Generic(_) => {
                todo!("we don't yet handle generics")
            }
            crate::ast::types::TypeBase::Resolved(_) => {
                //todo!("we shouldn't be trying to handle an already resolved typeref")
                // do nothing here, since maybe someone else resolved it?
            }
            crate::ast::types::TypeBase::UnResolved(ur) => {
                assert!(ur.generics.is_empty()); // we don't yet handle generics

                let r = self.resolve_symref(ur.named.clone()).await;

                let r = r.expect("handle bad imports");

                *base = TypeBase::Resolved(ast::types::ResolvedType {
                    from: ur.from,
                    base: r.is_at,
                    generics: ur.generics.clone(),
                })
            }
        }
    }

    async fn resolve_symref(&mut self, nr: ScopedName) -> Result<CompositeResolution, ImportError> {
        /*match self.name_to_ref.contains_key(&nr) {
            true => {
                // do nothing, already gonna resolve it
            }
            false => {
                let convo_id = self.next_convo();

                let cc = ConversationContext {
                    publish_as: None, // this isn't an import
                    remaining_scope: nr.clone().scope,
                    original_scope: nr.clone(),
                    // we search within parent here because we're a typeref within
                    // a Type, which implicitly looks for things within the parent
                    // scope unless we *explicitly* use the Self qualifier
                    searching_within: self.self_ctx.resolve().parent.unwrap(),
                    for_ref_id: convo_id,
                    public: false,
                };

                self.name_to_ref.insert(nr, convo_id);
                self.waiting_to_resolve.insert(convo_id, cc.clone());

                self.step_resolve(convo_id);
            }
        };*/
        warn!(
            "TypeResolver starts resolving {nr:?} within {:?}",
            self.node_id
        );
        self.earpiece.send(Message {
            to: Destination::resolver(self.node_id),
            from: self.as_dest(),
            send_reply_to: self.as_dest(),
            conversation: Uuid::new_v4(),
            content: Content::NameResolution(NameResolutionMessage::WhatIs {
                composite_symbol: nr,
                given_root: self
                    .node_id
                    .resolve()
                    .parent
                    .expect("there wasn't a parent node for a type"),
            }),
        });

        let m = self
            .earpiece
            .wait()
            .await
            .expect("didn't get a message back about imports");

        if let Content::NameResolution(nr) = m.content {
            match nr {
                NameResolutionMessage::RefersTo {
                    composite_symbol,
                    is_at,
                    given_root,
                } => Ok(CompositeResolution {
                    is_public: is_at.resolve().public,
                    name: composite_symbol,
                    is_at,
                }),
                NameResolutionMessage::HasNoResolution {
                    composite_symbol,
                    longest_prefix,
                    prefix_ends_at,
                    given_root,
                } => Err(todo!("proper handle of bad import")),
                _ => todo!("handle other nr messages"),
            }
        } else {
            todo!("handle other messages?");
        }

        //warn!("quark got a message back: {m:#?}");
    }
}
*/
