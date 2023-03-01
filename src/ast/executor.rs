use std::{
    cell::UnsafeCell,
    collections::{HashMap, VecDeque},
    mem::ManuallyDrop,
    pin::Pin,
    task::{RawWaker, RawWakerVTable, Wake},
};

use futures::{
    future::{BoxFuture, FutureExt, LocalBoxFuture},
    task::{waker_ref, ArcWake},
};
use tracing::{info, debug};
use std::{
    future::Future,
    sync::mpsc::{sync_channel, Receiver, SyncSender},
    sync::{Arc, Mutex},
    task::Context,
    time::Duration,
};

use uuid::Uuid;

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
    futures: UnsafeCell<HashMap<Uuid, Task<'static>>>,
}

impl Executor {
    /// CONTRACT: any task installed here must never, *ever*, await a future that could
    /// be completed by another thread, and must only await within the thread their
    /// poll() is called from, otherwise very bad things happen
    ///
    /// This may also only ever be called from within a single thread
    pub unsafe fn install(&self, future: impl Future<Output = ()> + 'static) {
        let future = future.boxed_local();
        let tid = Uuid::new_v4();

        let task = Task {
            future: UnsafeCell::new(future),
            //id: tid,
        };

        self.futures.get().as_mut().unwrap().insert(tid, task);

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

            let mref = unsafe {
                self.futures
                    .get()
                    .as_mut()
                    .unwrap()
                    .get_mut(&next_id)
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


            //debug!("starts poll for {next_id}");
            let res = mref.poll_unpin(&mut context);
            //debug!("finishes poll for {next_id}");

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
