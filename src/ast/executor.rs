use std::{
    cell::UnsafeCell,
    collections::{HashMap, VecDeque},
    marker::PhantomData,
    mem::ManuallyDrop,
    pin::Pin,
    rc::Rc,
    task::{RawWaker, RawWakerVTable, Wake},
};

use futures::future::{FutureExt, LocalBoxFuture};
use std::{future::Future, sync::Arc, task::Context};
use tracing::{debug, info};

use uuid::Uuid;

use crate::helper::SwapWith;

pub struct Task<'a> {
    //id: Uuid,
    future: UnsafeCell<LocalBoxFuture<'a, ()>>,
}

#[derive(Clone)]
pub struct MyWaker {
    queue: Pin<Arc<UnsafeCell<VecDeque<Uuid>>>>,
    id: Uuid,
}

impl Wake for MyWaker {
    fn wake(self: Arc<Self>) {
        info!("wake for task {} got called", self.id);
        unsafe { self.queue.get().as_mut().unwrap().push_back(self.id) }
    }

    fn wake_by_ref(self: &Arc<Self>) {
        self.clone().wake();
    }
}

// implementations (mostly) shamelessly stolen from tokio
impl MyWaker {
    unsafe fn clone_arc_raw(data: *const ()) -> RawWaker {
        let orig = Arc::from_raw(data as *const MyWaker);

        let c = orig.clone(); // increments ref count as well

        std::mem::forget(orig); // don't dec ref count from original

        let ptr = Arc::into_raw(c);

        RawWaker::new(ptr as *const (), Self::raw_waker_vtable())
    }

    unsafe fn wake_arc_raw(data: *const ()) {
        Arc::from_raw(data as *const MyWaker).wake()
    }

    unsafe fn wake_by_ref_arc_raw(data: *const ()) {
        let arc = ManuallyDrop::new(Arc::from_raw(data as *const MyWaker));
        Wake::wake_by_ref(&arc);
    }

    unsafe fn drop_arc_raw(data: *const ()) {
        drop(Arc::from_raw(data as *const MyWaker))
    }

    fn raw_waker_vtable() -> &'static RawWakerVTable {
        &RawWakerVTable::new(
            MyWaker::clone_arc_raw,
            MyWaker::wake_arc_raw,
            MyWaker::wake_by_ref_arc_raw,
            MyWaker::drop_arc_raw,
        )
    }
}

//unsafe impl<'rself> !Sync for Executor<'rself>;

pub struct Executor {
    queue: Pin<Arc<UnsafeCell<VecDeque<Uuid>>>>,
    futures: UnsafeCell<HashMap<Uuid, (Task<'static>, String)>>,
}

impl Executor {
    /// CONTRACT: any task installed here must never, *ever*, await a future that could
    /// be completed by another thread, and must only await within the thread their
    /// poll() is called from, otherwise very bad things happen
    ///
    /// This may also only ever be called from within a single thread
    pub unsafe fn install<IS: Into<String>>(
        &self,
        future: impl Future<Output = ()> + 'static,
        named: IS,
    ) {
        let named: String = named.into();

        let future = future.boxed_local();
        let tid = Uuid::new_v4();

        let task = Task {
            future: UnsafeCell::new(future),
            //id: tid,
        };

        info!("installing task with id {tid} named '{named}'");
        self.futures
            .get()
            .as_mut()
            .unwrap()
            .insert(tid, (task, named));

        unsafe {
            self.queue.as_ref().get().as_mut().unwrap().push_back(tid);
        }
    }

    /// Doesn't actually live for 'static, but if we're careful
    /// that none of the tasks try to run after we've been dropped that's ok
    ///
    /// since even if they do, they just have an arc to the waker, not to us
    pub unsafe fn new() -> Executor {
        Self {
            queue: Arc::pin(UnsafeCell::new(VecDeque::new())),
            futures: UnsafeCell::new(HashMap::new()),
        }
    }

    /// Runs all of the inner futures until all return Pending() and none
    /// have awoken during the last iter
    ///
    /// Returns true if we stepped any futures, false if no futures were queued
    pub fn until_stable(&self) -> bool {
        let mut stepped_any = false;

        while let Some(next_id) = unsafe { self.queue.as_ref().get().as_mut().unwrap().pop_front() }
        {
            //info!("stepping by id {next_id}");

            stepped_any = true;

            let mut named = String::new();

            let mref = unsafe {
                self.futures
                    .get()
                    .as_mut()
                    .unwrap()
                    .get_mut(&next_id)
                    .map(|(t, n)| {
                        //info!("going to poll task named '{n}', id'd {next_id}");
                        named = n.clone();
                        t
                    })
                    .unwrap()
                    .future
                    .get()
                    .as_mut()
                    .unwrap()
            };

            let waker = MyWaker {
                id: next_id,
                queue: self.queue.clone(),
            };

            let w = Arc::into_raw(Arc::new(waker)) as *const ();

            let rawwaker = RawWaker::new(w, MyWaker::raw_waker_vtable());

            let waker = unsafe { std::task::Waker::from_raw(rawwaker) };

            let mut context = Context::from_waker(&waker);

            debug!("starts poll for {next_id} named '{named}'");
            let res = mref.poll_unpin(&mut context);
            debug!("finishes poll for {next_id} named '{named}', it yielded back to us");

            match res {
                std::task::Poll::Ready(()) => {
                    // do nothing, the task is just done now
                    // so remove it from the task set
                    unsafe { self.futures.get().as_mut().unwrap().remove(&next_id) };
                }
                std::task::Poll::Pending => {
                    // keep it here I guess?
                }
            }
        }

        stepped_any
    }
}

pub struct UnsafeAsyncCompletableFuture<T: Clone> {
    _phantom: PhantomData<T>,
    refers: std::rc::Rc<UnsafeAsyncCompletable<T>>,
}

impl<T: Clone + std::fmt::Debug> Future for UnsafeAsyncCompletableFuture<T> {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> std::task::Poll<Self::Output> {
        tracing::debug!("got poll for unsafe completable");
        unsafe {
            match self.refers.value.get().as_ref().unwrap() {
                Some(v) => {
                    tracing::debug!("returns val {v:?} from unsafe completable");
                    std::task::Poll::Ready(v.clone())
                }
                None => {
                    tracing::debug!("future didn't have anything in it, so we rest");
                    let waker = cx.waker().clone();

                    self.refers.waiters.get().as_mut().unwrap().push(waker);

                    std::task::Poll::Pending
                }
            }
        }
    }
}

pub struct UnsafeAsyncCompletable<T: Clone> {
    value: UnsafeCell<Option<T>>,
    waiters: UnsafeCell<Vec<std::task::Waker>>,
}

impl<T: Clone + std::fmt::Debug> std::fmt::Debug for UnsafeAsyncCompletable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UnsafeAsyncCompletable")
            .field("value", &self.value)
            .field("waiters", &self.waiters)
            .finish()
    }
}

impl<T: Clone + std::fmt::Debug + 'static> UnsafeAsyncCompletable<T> {
    pub unsafe fn complete(&self, val: T) -> Result<(), SendError> {
        let mref = self.value.get().as_mut().expect("unsafecell being dumb");

        if mref.is_some() {
            return Err(SendError::AlreadyCompleted());
        }

        tracing::debug!("stores value into completable, val is {val:?}, wasn't already completed");

        *mref = Some(val);

        for waker in self
            .waiters
            .get()
            .as_mut()
            .expect("unsafecell being dumb")
            .swap_with(Vec::new())
            .into_iter()
        {
            waker.wake();
        }

        Ok(())
    }

    pub unsafe fn wait(self: Rc<Self>) -> UnsafeAsyncCompletableFuture<T> {
        if self.value.get().as_mut().expect("bad unsafecell").is_some() {
            //self.wake_waiters();
        }
        UnsafeAsyncCompletableFuture {
            _phantom: PhantomData::default(),
            refers: self.clone(),
        }
    }

    pub unsafe fn try_get(&self) -> Option<T> {
        self.value.get().as_ref().unwrap().clone()
    }

    pub unsafe fn new() -> Rc<Self> {
        Rc::new(Self {
            value: UnsafeCell::new(None),
            waiters: UnsafeCell::new(Vec::new()),
        })
    }

    /// CONTRACT: should never complete a or b directly after this, should only ever complete
    /// through the returned future if it exists
    pub unsafe fn combine<F, R>(
        within: &'static Executor,
        a: Rc<Self>,
        b: Rc<Self>,
        if_conflict: F,
    ) -> (Option<R>, Rc<Self>)
    where
        F: FnOnce(T, T) -> R,
    {
        unsafe {
            let v_1 = a.try_get();
            let v_2 = b.try_get();

            match (v_1, v_2) {
                (None, None) => {
                    // we could reuse one, but instead just make a new one that asserts that it is the
                    // only one to complete a and b

                    let nc = Self::new();

                    let ncf = nc.clone().wait();
                    let nf = within.install(
                        async move {
                            let v = ncf.await;

                            a.complete(v.clone()).unwrap();
                            b.complete(v).unwrap();
                        },
                        "unify two async completables that were incomplete at time of unify",
                    );

                    (None, nc)
                }
                (None, Some(v)) => {
                    // notify a using b
                    a.complete(v).unwrap();

                    (None, a)
                }
                (Some(v), None) => {
                    b.complete(v).unwrap();

                    (None, a)
                }
                (Some(va), Some(vb)) => {
                    let v = if_conflict(va, vb);
                    // would already be complete, so just return a
                    (Some(v), a)
                }
                //
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum SendError {
    AlreadyCompleted(),
}
