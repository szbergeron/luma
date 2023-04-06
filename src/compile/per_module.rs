use crate::{errors::CompilationError, mir::{transponster::Mediator, scribe::{Monomorphization, get_lines}}};
use futures::future::join_all;
use local_channel::mpsc::{Receiver as LocalReceiver, Sender as LocalSender};
use once_cell::sync::OnceCell;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tracing::{info, warn};
use uuid::Uuid;
//use std::collec
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::{
        atomic::{fence, AtomicIsize, AtomicUsize, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

use crate::{
    ast::{
        executor::{Executor, UnsafeAsyncCompletable, UnsafeAsyncCompletableFuture},
        resolver2::NameResolutionMessage,
        resolver2::Resolver,
        tree::CtxID,
    },
    avec::AtomicVecIndex,
    mir::{
        quark::Quark,
        transponster::{Memo, Transponster},
    },
};

pub struct CompilationUnit {
    /// we are solely responsible
    /// for dealing with the nodes within our domain.
    ///
    /// domains try to be clustered so that individual workers
    /// don't trample over each other, and so that
    /// when they "talk" to each other it is at a logical boundary at a parent spot in the tree
    domain: Vec<CtxID>,
}

impl CompilationUnit {
    pub fn new(domain: Vec<CtxID>) -> Self {
        Self { domain }
    }

    pub async fn launch(self, errors: UnboundedSender<CompilationError>) {
        info!("launching CompilationUnit across domain {:?}", self.domain);

        let mut senders = HashMap::new();
        let mut receivers = HashMap::new();

        for c in self.domain.iter() {
            let (send, recv) = tokio::sync::mpsc::unbounded_channel();
            senders.insert(*c, send);
            receivers.insert(*c, recv);
        }

        let postal = Arc::new(Postal::new(senders, errors));

        let static_postal = postal.clone();

        //POSTAL.set(static_postal).expect("couldn't set postal");

        Postal::set_instance(static_postal);

        fence(Ordering::SeqCst); // make sure that postal is set before director sees

        StalledDog::set_first_wait(self.domain.len()); // there are this many nodes that need to
                                                       // register before we should be waiting for
                                                       // a stall

        let postal2 = postal.clone();

        std::thread::spawn(|| {
            //std::thread::sleep(Duration::from_millis(200)); // give things time to boot up and send
            // initial messages
            //StalledDog::instance().watch();

            let d = Director {};
            d.entry(postal2);
        });

        let v: Vec<Group> = self
            .domain
            .into_iter()
            .map(|cid| {
                info!("creates a group with cid {cid:?}");
                let group = Group::new(receivers.remove(&cid).unwrap(), cid, postal.clone());

                group
            })
            .collect();

        println!("about to start watchdog");
        //tokio::spawn(Watchdog::new(postal.clone()).run());
        println!("started watchdog");

        std::thread::spawn(|| loop {
            std::thread::sleep(Duration::from_millis(1000));
            StalledDog::summarize();
        });

        join_all(v.into_iter().map(|r| {
            info!("going to start cid group {:?}", r.for_node);
            r.start()
        }))
        .await;
    }
}

pub struct Watchdog {
    postal: Arc<Postal>,
}

impl Watchdog {
    pub fn new(p: Arc<Postal>) -> Self {
        Self { postal: p }
    }

    pub async fn run(self) {
        let mut sent_fuse = false;
        println!("about to send heartbeat");
        while self.postal.send_heartbeat() {
            println!("sent heartbeat");
            tokio::time::sleep(Duration::from_millis(5000)).await;

            if !sent_fuse {
                sent_fuse = true;
                self.postal.send_broadcast(Message {
                    to: Destination {
                        node: CtxID(AtomicVecIndex::nil()),
                        service: Service::Broadcast(),
                    },
                    from: Destination::nil(),
                    send_reply_to: Destination::nil(),
                    conversation: uuid::Uuid::new_v4(),
                    content: Content::Control(ControlMessage::ShouldFuseNames()),
                })
            }
        }
    }
}

pub struct StalledDog {
    epoch: AtomicUsize,
    quark_progress: AtomicUsize,
    transponster_progress: AtomicUsize,

    live_messages: AtomicIsize,
    awake: AtomicUsize,
    first_wait: AtomicUsize,
    //zero_events: OnceCell<(Sender<()>, Receiver<()>)>,
}

static STALLED_DOG: StalledDog = StalledDog::new();

impl StalledDog {
    pub const fn new() -> Self {
        Self {
            epoch: AtomicUsize::new(1),
            quark_progress: AtomicUsize::new(1),
            transponster_progress: AtomicUsize::new(1),
            live_messages: AtomicIsize::new(0),
            awake: AtomicUsize::new(0),
            first_wait: AtomicUsize::new(0),
        }
    }

    pub fn nudge_epoch() {
        Self::instance()
            .epoch
            .fetch_add(1, std::sync::atomic::Ordering::Release);
    }

    pub fn nudge_quark() {
        //println!("Quark made progress");
        Self::instance()
            .quark_progress
            .fetch_add(1, std::sync::atomic::Ordering::Release);
    }

    fn message_update(delta: isize) {
        Self::instance()
            .live_messages
            .fetch_add(delta, Ordering::AcqRel);
    }

    pub fn inc_live_message() {
        Self::message_update(1);
    }

    pub fn dec_live_message() {
        Self::message_update(-1);
    }

    pub fn wake_one() {
        Self::instance().awake.fetch_add(1, Ordering::AcqRel);
    }

    pub fn sleep_one() {
        let inst = Self::instance();
        let prior = inst.awake.fetch_sub(1, Ordering::AcqRel);

        let now = prior - 1;

        if now == 0 {
            // could add code to do a notify through channel on this event
        }
    }

    pub fn notify_started() {
        Self::instance().first_wait.fetch_sub(1, Ordering::Release);
    }

    pub fn set_first_wait(val: usize) {
        Self::instance().first_wait.store(val, Ordering::SeqCst);
    }

    /// Blocks until node network has booted
    pub fn wait_node_startup(&self) {
        loop {
            let nodes_first_run = self.first_wait.load(Ordering::Acquire);

            if nodes_first_run > 0 {
                continue;
            } else {
                return;
            }
        }
    }

    pub fn summarize() {
        let i = Self::instance();
        let e = i.epoch.load(Ordering::SeqCst);
        let l = i.live_messages.load(Ordering::SeqCst);
        let a = i.awake.load(Ordering::SeqCst);

        println!("Current state: {e} epoch, {l} live, {a} awake");
    }

    fn wait_stalled(&self) {
        loop {
            std::thread::sleep(Duration::from_micros(50)); // basically an annoying spinlock
                                                           //let prior_epoch = self.epoch.load(std::sync::atomic::Ordering::Acquire);

            let awake = self.awake.load(std::sync::atomic::Ordering::Acquire);

            if awake == 0 {
                let live = self.live_messages.load(Ordering::Acquire);

                if live == 0 {
                    if self.check_stalled() {
                        break;
                    }
                }

                continue;
            } else {
                continue;
            }
        }
    }

    /// Our early warning has fired, so we check here with an expensive check cycle
    /// This means that both the number of messages that are live was zero and in close proximity
    /// the number of awake nodes was zero
    fn check_stalled(&self) -> bool {
        let prior_epoch = self.epoch.load(Ordering::SeqCst);

        let awake = self.awake.load(Ordering::SeqCst);
        let live_messages = self.live_messages.load(Ordering::SeqCst);

        let later_epoch = self.epoch.load(Ordering::SeqCst);

        if awake == 0 && live_messages == 0 && prior_epoch == later_epoch {
            //panic!("system is stalled");
            return true;
        } else {
            return false;
        }
    }

    fn instance() -> &'static Self {
        &STALLED_DOG
    }
}

struct Director {
    //
}

impl Director {
    pub fn entry(&self, postal: Arc<Postal>) {
        let begin = Instant::now();

        tracing::info!("entry for director");
        let st = StalledDog::instance();
        // first, wait for entire network to start up
        st.wait_node_startup();

        // now, wait for the very first stall and converge
        st.wait_stalled();
        tracing::info!("finished first stall wait");

        // now enter the bi-phasic wait
        loop {
            tracing::info!("Starting a loop for phase");
            let qk_before = st.quark_progress.load(Ordering::SeqCst);
            let tp_before = st.transponster_progress.load(Ordering::SeqCst);

            postal.send_broadcast_to(Service::Quark(), Content::Quark(Photon::BeginPhase()));

            // wait for quark to finish phase
            st.wait_stalled();
            tracing::info!("Quark stalled for phase");

            // send phase change notification
            postal.send_broadcast_to(Service::Quark(), Content::Quark(Photon::EndPhase()));

            st.wait_stalled();

            postal.send_broadcast_to(
                Service::Transponster(),
                Content::Transponster(Memo::BeginPhase()),
            );

            // wait for transponster to finish phase and elect/unify some field
            st.wait_stalled();

            // commit any fields that exist
            let committed_fields = Mediator::instance().run_election();

            if committed_fields {
                tracing::info!("wooo! committed a field");
            }

            // wait for transponster to deal with the commits
            st.wait_stalled();
            tracing::info!("Transponster stalled for phase");

            let qk_after = st.quark_progress.load(Ordering::SeqCst);

            if qk_before == qk_after && !committed_fields {
                tracing::info!(
                    "The system has reached a converged final state, tell it to emit any errors"
                );

                postal.send_broadcast_to(Service::Transponster(), Content::Transponster(Memo::CompilationStalled()));

                st.wait_stalled();

                postal.send_broadcast_to(
                    Service::Quark(),
                    Content::Quark(Photon::CompilationStalled()),
                );

                st.wait_stalled();


                // now, if no errors, start codegen

                for service in [Service::Quark(), Service::Transponster()] {
                    postal.send_broadcast_to(service, Content::StartCodeGen());
                }

                st.wait_stalled(); // everyone has codegen now, so emit output
                
                for line in get_lines() {
                    println!("{line}");
                }
                //postal.send_broadcast_to(Service::Quark(), Content::Quark(Photon::StartCodeGen()));

                //std::thread::sleep(Duration::from_secs(10));

                tracing::info!("Exiting system");

                let end = Instant::now();

                let delta = end - begin;
                let micros = delta.as_micros();

                std::thread::sleep(Duration::from_millis(200));

                println!("Phase section took {} microseconds", micros);

                // send notifications to all nodes to ask them to emit any late errors
                std::thread::sleep(Duration::from_secs(2));
                std::process::exit(0);
            } else {
                //println!("Values for qk and such: {qk_before}, {qk_after}, {tp_before}, {tp_after}");
            }

            // send message to quark to begin phase
        }
    }
}

/// A PostalWorker handles sending messages between Resolvers :)
#[derive(Debug)]
pub struct Postal {
    senders: HashMap<CtxID, tokio::sync::mpsc::UnboundedSender<Message>>,
    errors: UnboundedSender<CompilationError>,
    exited: AtomicUsize,
}

impl Postal {
    pub fn new(
        senders: HashMap<CtxID, UnboundedSender<Message>>,
        errors: UnboundedSender<CompilationError>,
    ) -> Self {
        Self {
            errors,
            senders,
            exited: AtomicUsize::new(0),
        }
    }

    /*pub fn send_reply(&self, from: CtxID, to: CtxID, reply: Reply) {
        self.senders
            .get(&to)
            .unwrap()
            .send(Message::Reply(reply))
            .unwrap();
    }

    pub fn send_ask(&self, from: CtxID, to: CtxID, request: Request) {
        self.senders
            .get(&to)
            .unwrap()
            .send(Message::Request(request))
            .unwrap();
    }*/

    pub fn send(&self, msg: Message) {
        if let Content::Error(e) = msg.content {
            let _ = self.errors.send(e);
        } else {
            match self.senders.get(&msg.to.node) {
                Some(v) => {
                    StalledDog::inc_live_message();
                    v.send(msg).expect("couldn't send a message through postal");
                }
                None => {
                    tracing::error!("something tried to send to a nil dest! message: {msg:?}");
                }
            }
        }
    }

    pub fn sign_out(&self, id: CtxID) {
        println!("{id:?} signs out");
        let new_v = self
            .exited
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
            + 1;

        println!("new signed-in count is {new_v}");

        if new_v == self.senders.len() {
            println!("exiting everyone");
            // everyone has signed out
            for (ctxid, sender) in self.senders.iter() {
                //sender.send(Message::Exit()).unwrap();
                let _ = sender.send(Message {
                    to: Destination {
                        node: *ctxid,
                        service: Service::Broadcast(),
                    },
                    from: Destination::nil(),
                    send_reply_to: Destination::nil(),
                    content: Content::Control(todo!()),
                    conversation: Uuid::new_v4(),
                });
            }
        }
    }

    pub fn send_broadcast(&self, msg: Message) {
        for (cid, send) in self.senders.iter() {
            let mut msg = msg.clone();
            msg.to = Destination {
                node: *cid,
                service: Service::Broadcast(),
            };

            self.send(msg);
        }
    }

    pub fn send_broadcast_to(&self, service: Service, content: Content) {
        for (cid, send) in self.senders.iter() {
            let msg = Message {
                to: Destination {
                    node: *cid,
                    service,
                },
                from: Destination {
                    node: CtxID(AtomicVecIndex::nil()),
                    service: Service::Broadcast(),
                },
                send_reply_to: Destination::nil(),
                conversation: Uuid::new_v4(),
                content: content.clone(),
            };

            self.send(msg);
        }
    }

    pub fn exited(&self) -> bool {
        self.exited.load(std::sync::atomic::Ordering::SeqCst) == self.senders.len()
    }

    /// returns true if heartbeat was sent successfully,
    /// false if everyone has exited
    pub fn send_heartbeat(&self) -> bool {
        return true; // just for now

        if self.exited() {
            false
        } else {
            for (&ctxid, sender) in self.senders.iter() {
                let _ = sender.send(Message {
                    to: Destination {
                        node: ctxid,
                        service: Service::Broadcast(),
                    },
                    from: Destination::nil(),
                    send_reply_to: Destination::nil(),
                    content: Content::Control(ControlMessage::CheckIn()),
                    conversation: Uuid::new_v4(),
                });
            }

            true
        }
    }

    pub fn set_instance(v: Arc<Self>) {
        POSTAL.try_insert(v).expect("couldn't set postal");
    }

    pub fn instance() -> Arc<Self> {
        POSTAL.get().unwrap().clone()
    }

    pub fn send_and_forget(&self, to: Destination, content: Content) {
        self.send(Message { to, from: Destination::nil(), send_reply_to: Destination::nil(), conversation: Uuid::new_v4(), content })
    }
}

lazy_static! {
    static ref POSTAL: OnceCell<Arc<Postal>> = OnceCell::new();
}

pub struct Group {
    listen_to: UnboundedReceiver<Message>,
    for_node: CtxID,
    with_postal: Arc<Postal>,
}

fn static_pinned_leaked<V>(v: V) -> &'static V {
    //let boxed = Box::pin(v); // don't even pin here, just senseless
    let mref = Box::leak(Box::new(v));

    mref
}

impl Group {
    pub fn new(
        listen_to: UnboundedReceiver<Message>,
        for_node: CtxID,
        with_postal: Arc<Postal>,
    ) -> Self {
        Self {
            listen_to,
            for_node,
            with_postal,
        }
    }
    pub async fn start(mut self) {
        tracing::error!("starts group for node {:?}", self.for_node);
        // this is only allowed because the things that *aren't* send
        // in these futures actually don't care so long as
        // the entire group of them is moved at a time
        let exe = unsafe { Executor::new() };
        let exer = static_pinned_leaked(exe);

        let (res_s, res_r) = local_channel::mpsc::channel();
        let (qk_s, qk_r) = local_channel::mpsc::channel();
        let (ocl_s, ocl_r) = local_channel::mpsc::channel();
        let (rtr_s, rtr_r) = local_channel::mpsc::channel();

        let (send_out_s, mut send_out_r) = lockfree::channel::spsc::create();
        //let (br_s, br_r) = local_channel::mpsc::channel();
        let resolver = Resolver::for_node(
            self.for_node,
            exer,
            Earpiece::new(rtr_s.clone(), res_r, self.for_node),
        );

        /*let resolver_pinned = Box::pin(resolver);
        let resolver_static_ptr = Pin::into_inner(resolver_pinned.as_ref()) as *const Resolver;
        let resolver_static_ref = unsafe { resolver_static_ptr.as_ref().unwrap() };*/
        let resolver_static_ref = static_pinned_leaked(resolver);

        let quark = Quark::for_node(
            self.for_node,
            rtr_s.clone(),
            //Earpiece::new(rtr_s.clone(), qk_r, self.for_node),
            exer,
        );

        //let bridge = Bridge::new(self.listen_to, rtr_s.clone());
        let router = Router::new(
            rtr_r,
            vec![
                (Service::Resolver(), res_s),
                (Service::Transponster(), ocl_s),
                (Service::Quark(), qk_s),
                (Service::Router(), rtr_s.clone()),
            ],
            send_out_s,
            //self.with_postal,
            //
            self.for_node,
        );

        let oracle = Transponster::for_node(
            self.for_node,
            Earpiece::new(rtr_s.clone(), ocl_r, self.for_node),
        );

        unsafe {
            info!("installing resolver uses into exe");
            resolver_static_ref.install(exer);
            info!("installed resolver uses into exe");

            // it doesn't truly live for static, but it lives long enough that the executor itself
            // is dropped before resolver is, and we've awaited all the futures so they are allowed
            // to drop then
            //let pinned = Box::pin(resolver);

            /*exer.install(async {
                panic!("woo")
            }, "blah");*/

            exer.install(
                resolver_static_ref.thread(),
                format!("resolver for node {:?}", self.for_node),
            );

            exer.install(
                quark.thread(Earpiece::new(rtr_s.clone(), qk_r, self.for_node)),
                format!("quark for node {:?}", self.for_node),
            );
            exer.install(
                oracle.thread(exer),
                format!("oracle for node {:?}", self.for_node),
            );

            exer.install(
                router.thread(),
                format!("router for node {:?}", self.for_node),
            );

            StalledDog::wake_one();
            StalledDog::notify_started();

            //exe.install(bridge.thread());

            // we don't use Bridge anymore, since doing the bridge
            // operation actually requires knowing about the executor
            loop {
                while let Ok(m) = self.listen_to.try_recv() {
                    // we took one out, so push it into router
                    StalledDog::dec_live_message();
                    rtr_s.send(m).expect("couldn't push into rtr?");
                }

                info!("doing a step loop");
                // we want to wait until everything internal stabilizes,
                // and then only step one external message at a time
                let stepped_any = exer.until_stable(); // not async since it is, itself, an executor
                                                       //
                if stepped_any {
                    // we haven't been stalled this round, nudge before sleeping again
                    //println!("stepped some internally");
                    StalledDog::nudge_epoch();
                }

                // now everything is sleeping, so long as the
                // services are well behaved (we have to assume they are!),
                // the only thing that could possibly wake any of them up is a message from outside

                // so, wait for a message from outside, and potentially yield
                // to other nodes

                info!("finished stepping, until_stable returned true");

                // make sure we send out any messages waiting to go out
                // rely on this to not be a blocking channel
                info!("sending out all outgoing messages");
                while let Ok(m) = send_out_r.recv() {
                    // add 1 live message since we're sending one out
                    //StalledDog::message_update(1);
                    self.with_postal.send(m);
                }

                info!("sent all outgoing messages");

                // go to sleep before waiting on new messages
                StalledDog::sleep_one();
                match self.listen_to.recv().await {
                    Some(m) => {
                        StalledDog::wake_one();
                        // we took one out, so push it into router
                        StalledDog::message_update(-1);
                        rtr_s.send(m).expect("couldn't push into rtr?");
                    }
                    None => todo!(),
                }
            }
        }
    }

    /*pub fn send(&self, msg: &Message) {}

    pub fn recv(&self, me: &Destination) -> Result<Message, ()> {
        todo!()
    }*/
}

pub struct Bridge {
    listen_to: UnboundedReceiver<Message>,
    send_to: LocalSender<Message>,
}

impl Bridge {
    pub async fn thread(mut self) {
        while let Some(v) = self.listen_to.recv().await {
            let _ = self.send_to.send(v);
        }
    }

    pub fn new(listen_to: UnboundedReceiver<Message>, send_to: LocalSender<Message>) -> Self {
        Self { listen_to, send_to }
    }
}

pub struct Router {
    listen_to: LocalReceiver<Message>,
    send_on: Vec<(Service, LocalSender<Message>)>,
    send_out: lockfree::channel::spsc::Sender<Message>,
    for_node: CtxID,
}

impl Router {
    pub fn new(
        listen_to: LocalReceiver<Message>,
        send_on: Vec<(Service, LocalSender<Message>)>,
        send_out: lockfree::channel::spsc::Sender<Message>,
        for_node: CtxID,
    ) -> Self {
        Self {
            listen_to,
            send_on,
            send_out,
            for_node,
        }
    }

    pub async fn thread(mut self) {
        info!("starting router thread");
        while let Some(v) = self.listen_to.recv().await {
            warn!("router got a message");
            if v.to.node == self.for_node {
                warn!("got message for local, message is {v:#?}");
                // the message is for a local service
                match v.to.service {
                    Service::Broadcast() => {
                        //panic!("broadcast?");

                        //StalledDog::dec_live_message(); // we "consume" this message

                        info!("it was a broadcast message");
                        for (serv, send) in self.send_on.iter_mut() {
                            if *serv == Service::Router() {
                                // don't send a broadcast right back to the thing that sent it
                                info!("avoiding sending to self");
                                continue;
                            } else {
                                info!("sends to dest {:?}", v.from);
                                let res = send.send(v.clone());

                                if let Ok(_) = res {
                                    // sent a message successfully, so another live one
                                    // is introduced, but it isn't "on the internode network"
                                    // so it isn't tracked (really)
                                }
                            }
                        }
                    }
                    Service::Controller() => {
                        //
                    }
                    other => {
                        let (srv, snd) = self
                            .send_on
                            .iter_mut()
                            .find(|v| v.0 == other)
                            .expect("couldn't find a matching value for service");

                        let _ = snd.send(v);
                    }
                }
            } else {
                // message is going out to another node
                warn!("sending message {v:?} out from router");
                self.send_out
                    .send(v)
                    .expect("couldn't send out from router");
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Message {
    /// The intended recipient of this message
    pub to: Destination,

    /// The originator of this message
    pub from: Destination,

    /// Where a reply to this message should be directed to
    pub send_reply_to: Destination,

    /// States what message chain this is part of
    pub conversation: Uuid,

    pub content: Content,
}

#[derive(Debug, Clone)]
pub enum Content {
    Control(ControlMessage),

    Announce(AnnounceMessage),

    Quark(Photon),

    Transponster(Memo),

    NameResolution(NameResolutionMessage),

    Error(CompilationError),

    Monomorphization(Monomorphization),

    StartCodeGen(),
}

#[derive(Debug, Clone)]
pub enum AnnounceMessage {
    PhaseComplete(Service, Phase),
}

/// How Quarks qtalk to each other :P
#[derive(Debug, Clone)]
pub enum Photon {
    /// terminate the current phase,
    /// and collect and emit related errors
    EndPhase(),

    /// Start using inputs from new facts to do
    /// type resolution--this doesn't
    /// do a whole lot directly,
    /// but is a counterpart to the Transponster
    /// version of the same Begin/End pair
    BeginPhase(),

    /// After this point, if any typevars are Free(),
    /// they will never be unified so the program does not typecheck
    CompilationStalled(),

    StartCodeGen(),
}

#[derive(Debug, Clone)]
pub enum ControlMessage {
    /// Analogue for a heartbeat message
    CheckIn(),

    /// A reply from a node where the service is inactive,
    /// so no reply can be expected
    ///
    /// Can be used to indicate that, say, the type of a field was asked of
    /// a
    CanNotReply(),

    ShouldFuseNames(),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Destination {
    pub node: CtxID,

    pub service: Service,
}

impl Destination {
    /// Yields a destination that seems to come from "nowhere",
    /// corresponds to injecting a message from outside the system
    pub fn nil() -> Self {
        Self {
            node: CtxID(AtomicVecIndex::nil()),
            service: Service::Broadcast(),
        }
    }

    pub fn resolver(node: CtxID) -> Self {
        Self {
            node,
            service: Service::Resolver(),
        }
    }

    pub fn transponster(node: CtxID) -> Self {
        Self {
            node,
            service: Service::Transponster(),
        }
    }

    pub fn quark(node: CtxID) -> Self {
        Self {
            node,
            service: Service::Quark(),
        }
    }

    pub fn mediator() -> Self {
        Destination {
            service: Service::Mediator(),
            ..Self::nil()
        }
    }
}

pub struct Earpiece {
    listen: local_channel::mpsc::Receiver<Message>,

    //shout: Arc<Postal>,
    talk: local_channel::mpsc::Sender<Message>,

    within: CtxID,
}

impl Earpiece {
    pub fn send(&mut self, message: Message) {
        /*if message.to.node == self.within {
            let _ = self.whisper.send(message);
        } else {
            self.shout.send(message);
        }*/
        let _ = self.talk.send(message);
    }

    pub async fn wait(&mut self) -> Result<Message, ()> {
        self.listen.recv().await.ok_or(())
    }

    pub fn new(send: LocalSender<Message>, recv: LocalReceiver<Message>, within: CtxID) -> Self {
        Self {
            listen: recv,
            talk: send,
            within,
        }
    }

    pub fn split(
        self,
    ) -> (
        local_channel::mpsc::Receiver<Message>,
        local_channel::mpsc::Sender<Message>,
    ) {
        let Self {
            listen,
            talk,
            within,
        } = self;

        (listen, talk)
    }

    pub fn cloned_sender(&self) -> local_channel::mpsc::Sender<Message> {
        self.talk.clone()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Service {
    Resolver(),
    Quark(),
    Router(),
    Transponster(),

    Mediator(),

    /// A message specifically for the controller on a node,
    /// used for things like upward phase announcements
    Controller(),

    /// Should be copied and sent to every service on a node
    Broadcast(),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Phase {
    /// Says that initial bringup is complete,
    /// all data structures have been initialized
    /// and we are ready to begin actually dealing
    /// with new messages as they come in
    Init(),

    /// Means that we have finished all of our local resolution,
    /// and have entered a steady-state reply state
    LocallyComplete(),

    /// Means that we have finished shutting down our
    /// message handler, and have finished any work
    /// that we were expected to do for external messages
    GloballyComplete(),
}

pub struct ConversationContext {
    inner: RefCell<ConversationContextInner>,
}

struct ConversationContextInner {
    waiters: HashMap<Uuid, Rc<UnsafeAsyncCompletable<Message>>>,
    sender: local_channel::mpsc::Sender<Message>,
}

impl ConversationContext {
    /// CONTRACT: this may be passed between threads, but must never
    /// be shared between them despite the type being Sync + Send
    /// and the main methods taking &self
    pub unsafe fn new(sender: local_channel::mpsc::Sender<Message>) -> Self {
        Self {
            inner: RefCell::new(ConversationContextInner {
                waiters: HashMap::new(),
                sender,
            }),
        }
    }

    /// If the message wasn't for an active conversation,
    /// then the message is returned intact as Some(Message)
    ///
    /// Otherwise, the message was handled internally
    pub fn dispatch(&self, message: Message) -> Option<Message> {
        let mut inner = self.inner.borrow_mut();

        if let Some(v) = inner.waiters.remove(&message.conversation) {
            unsafe {
                v.complete(message).expect("already complete?");
            }

            None
        } else {
            Some(message)
        }
    }

    pub fn send_and_forget(&self, message: Message) {
        let inner = self.inner.borrow_mut();

        inner
            .sender
            .send(message)
            .expect("couldn't send, this is bad?");
    }

    pub fn wait_for(&self, reply_to: Message) -> UnsafeAsyncCompletableFuture<Message> {
        let mut inner = self.inner.borrow_mut();

        let fut = unsafe { UnsafeAsyncCompletable::new() };

        inner.waiters.insert(reply_to.conversation, fut.clone());

        inner.sender.send(reply_to).expect("couldn't send?");

        std::mem::drop(inner); // make extra sure we drop the refmut

        unsafe { fut.wait() } // don't await it, the user can do that for themselves
    }
}
