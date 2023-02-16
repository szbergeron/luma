use tokio::task::LocalSet;

use crate::{ast::resolver::Resolver, mir::quark::Quark};

pub struct Group {
    resolver: Resolver,
    quark: Quark,
    handler: Handler,
}

impl Group {
    pub async fn thread(mut self) {
        let local = LocalSet::new();

        local.spawn_local(self.resolver.thread());
        local.spawn_local(self.quark.thread());
        local.spawn_local(self.handler());

        local.await
    }
}
