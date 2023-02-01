use std::{
    alloc::{self, Layout},
    fmt::Display,
    marker::PhantomData,
    mem::MaybeUninit,
    ptr::{addr_of_mut, null_mut},
    sync::atomic::{AtomicPtr, AtomicUsize, fence},
};

struct ChunkEntry<T> {
    //unatomic: UnsafeCell<*mut T>,
    lockfree: AtomicPtr<MaybeUninit<T>>,

    blocking: std::sync::Mutex<Option<*mut MaybeUninit<T>>>,

    len: AtomicUsize,

    capacity: usize,
}

impl<T> ChunkEntry<T> {
    /// Used to get a value at a specific index that
    /// is known to have already been initialized.
    ///
    /// Behavior is undefined/invalid if index has not already
    /// been initialized
    pub unsafe fn get(&self, index: usize) -> &T {
        if index >= self.len.load(std::sync::atomic::Ordering::Acquire) {
            panic!("Improper usage of ChunkEntry::get()");
        }

        let r = self.touch();

        let optr = r.offset(index as isize);

        fence(std::sync::atomic::Ordering::Acquire);
        (*optr).assume_init_ref()
        //if self.unatomic.get()
    }

    /// This is the only way to put an element into
    /// a Chunk, and must not panic. It will increase the size,
    /// and assumes that we are never inserting over
    /// top of an already inserted element
    pub unsafe fn place(&self, index: usize, t: T) {
        if index >= self.capacity {
            panic!("Improper usage of ChunkEntry::place()");
        }

        (*self.touch().offset(index as isize)) = MaybeUninit::new(t);

        self.len.fetch_add(1, std::sync::atomic::Ordering::Release);
    }

    /// "touches" a chunk, so if it has
    /// no backing store then one will be allocated.
    /// This also tries to ensure that
    unsafe fn touch(&self) -> *mut MaybeUninit<T> {
        let lockfree = self.lockfree.load(std::sync::atomic::Ordering::Acquire);

        if lockfree.is_null() {
            self.try_alloc()
        } else {
            lockfree
        }
    }

    unsafe fn try_alloc(&self) -> *mut MaybeUninit<T> {
        /*let guard = self.blocking.lock() {
        };*/

        let mut guard = self.blocking.lock().unwrap();

        match *guard {
            #[allow(unused_unsafe)]
            Some(inner) => unsafe { std::mem::transmute::<_, *mut MaybeUninit<T>>(inner) },
            None => {
                let allocation =
                    std::alloc::alloc(Layout::array::<MaybeUninit<T>>(self.capacity).unwrap());

                let allocation = std::mem::transmute::<_, *mut MaybeUninit<T>>(allocation);

                //let pinned_allocation = Pin::new_unchecked(allocation);

                *guard = Some(allocation);

                allocation
            }
        }
    }

    //fn get_inner<'s>(&'s self, backing: &'s )
}

impl<T> Drop for ChunkEntry<T> {
    fn drop(&mut self) {
        // we can assume by the contracts of ChunkEntry that
        // if len has been spanned across an index (and no panic has occurred on insert, which
        // is a guarantee that must be fulfilled by users of the type), then that index is
        // initialized and should be deallocated
        let mut guard = self.blocking.lock().unwrap();

        let alloc = guard.replace(null_mut());

        match alloc {
            None => (), // nothing to drop, chunk was never touched
            Some(alloc) => {
                // drop consumes self, so since all writes will
                // have been with AcqRel and we know that insert
                // cannot panic between modifying len and
                // swapping init value, then all cells within len
                // must be init
                let len = self.len.load(std::sync::atomic::Ordering::Acquire);

                for i in 0..len {
                    unsafe {
                        //std::mem::drop((*alloc.offset(i as isize)).assume_init());
                        //Box::from_raw((alloc.offset(i as isize)).assume_init());
                        let p = alloc.offset(i as isize);
                        //let init = &(*p).assume_init();
                        let as_t_ptr: *mut T = std::mem::transmute(p);
                        let b = Box::from_raw(as_t_ptr);
                        std::mem::drop(b);
                    }
                }

                unsafe {
                    std::alloc::dealloc(
                        std::mem::transmute(alloc),
                        Layout::array::<MaybeUninit<T>>(self.capacity).unwrap(),
                    );
                }
            }
        }
    }
}

//#[repr(C)]
struct Chunk<T> {
    //size: usize,
    cur: AtomicUsize,
    content: [MaybeUninit<T>],
}

impl<T> Chunk<T> {
    /// Based on code provided by Plecra#5251 for fat ptr handling, declared public domain (MIT
    /// compatible)
    pub fn with_capacity(num: usize) -> *mut Self {
        let layout = unsafe {
            std::alloc::Layout::for_value_raw(std::ptr::slice_from_raw_parts(
                std::ptr::null::<()>(),
                num,
            ) as *const Self)
        };

        let ptr = unsafe { alloc::alloc(layout) };
        if ptr.is_null() {
            alloc::handle_alloc_error(layout);
        }

        let ptr = std::ptr::slice_from_raw_parts_mut(ptr, num) as *mut Self;
        unsafe {
            addr_of_mut!((*ptr).cur).write(AtomicUsize::new(0));
            //Box::from_raw(ptr)
            ptr
        }
    }

    pub unsafe fn make_fat_ptr(r: *mut (), len: usize) -> *mut Self {
        //println!("makes fat ptr with len {}", len);
        //according to https://doc.rust-lang.org/nightly/core/ptr/trait.Pointee.html, the
        //metadata of the last field is the metadata for the fat ptr used
        let v: *mut Self = std::ptr::from_raw_parts_mut(r, len);

        //println!("v len is {}", (*v).content.len());

        v
        /*let conv: *mut T = std::mem::transmute(r);
        let r: *mut [T] = core::slice::from_raw_parts_mut(conv, len);
        r as *mut Self*/
    }
}

#[repr(C)]
#[allow(dead_code)]
#[derive(Debug)]
struct SizedChunk<T> {
    size: usize,
    cur: AtomicUsize,
    content: [MaybeUninit<T>; 1],
}

#[derive(Debug)]
pub struct AtomicVec<T, const CHUNK_COUNT: usize = 32, const FIRST_CHUNK_SIZE: usize = 32> {
    self_key: usize,
    chunks: [AtomicPtr<()>; CHUNK_COUNT],
    lengths: [usize; CHUNK_COUNT],
    cur: AtomicUsize,
    phantom: PhantomData<T>,
}

#[allow(dead_code)]
struct ImplDefaultConstArray<T: Default, const N: usize> {
    arr: MaybeUninit<[T; N]>,
    idx: usize,
}

impl<T, const CC: usize, const FCS: usize> AtomicVec<T, CC, FCS> {
    fn make_lengths<const COUNT: usize>() -> [usize; COUNT] {
        let mut c = [0; COUNT];

        let mut cur_size = FCS;

        for i in 0..c.len() {
            c[i] = cur_size;
            cur_size *= 2;
        }

        c
    }

    pub fn new() -> AtomicVec<T, CC, FCS> {
        static INIT_KEY: AtomicUsize = AtomicUsize::new(0);
        //const nptr: AtomicPtr<()> = AtomicPtr::new(null_mut());

        /*let mut chunks = Vec::new();
        for i in 0..32 {
            chunks.push(AtomicPtr::new(null_mut()));
        }

        //let (slice, _, _) = chunks.into_raw_parts();*/

        AtomicVec {
            self_key: INIT_KEY.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            //chunks: Default::default(), // null ptrs
            //chunks: [Default::default(); CC],
            //chunks: [AtomicPtr<()>; CHUNK_COUNT].from
            //chunks: Self::make_chunks_default(),
            //chunks,
            chunks: [(); CC].map(|_| Default::default()),
            cur: AtomicUsize::new(0),
            phantom: Default::default(),
            lengths: Self::make_lengths(),
        }
    }

    /// returns a non-null ptr to a chunk
    fn get_chunk(&self, idx: usize) -> *const Chunk<T> {
        // check first if it was already non-null
        let ptr = self.chunks[idx].load(std::sync::atomic::Ordering::Acquire);

        let r = if ptr.is_null() {
            // do alloc speculatively since it was null at first load
            let size = (2 as usize).pow(idx as u32) * FCS;

            let fat_ptr: *mut Chunk<T> = Chunk::with_capacity(size);

            //core::slice::
            let (ptr, _meta) = fat_ptr.to_raw_parts();

            //let pinned = Pin::new(Chunk::with_capacity(size));
            //

            let res = self.chunks[idx]
                // both are AcqRel because I don't have the mental capacity to figure out if
                // anything else is safe right now. Doesn't need SeqCst since threads only care
                // about their own load/store so no "3rd" thread is involved in any interaction
                // here
                .compare_exchange(
                    null_mut(),
                    ptr,
                    std::sync::atomic::Ordering::AcqRel,
                    std::sync::atomic::Ordering::Acquire,
                );

            //println!("had to do a chunk alloc");

            match res {
                Ok(_p) => {
                    //println!("We could write ours");
                    // we got a chance to write our ptr, no other thread did it faster
                    ptr
                }
                Err(old) => {
                    // we need to dealloc ourself since some other thread already wrote the value
                    //println!("Other thread got there first, need to use theirs");
                    unsafe {
                        let b = Box::from_raw(fat_ptr);
                        drop(b);
                    }

                    old
                }
            }
        } else {
            ptr
        };

        unsafe { Chunk::make_fat_ptr(r, self.lengths[idx]).as_ref().unwrap() }
    }

    fn try_insert_chunk<'c>(
        &self,
        chunk_idx: i32,
        chunk: &'c Chunk<T>,
        val: &mut Option<T>,
    ) -> Option<(AtomicVecIndex, &'c mut T)> {
        let idx = chunk.cur.fetch_add(1, std::sync::atomic::Ordering::AcqRel); // bump idx speculatively

        if idx >= chunk.content.len() {
            //println!("size of chunk wouldn't allow putting in obj, size was {} while idx was {}", chunk.content.len(), idx);
            None // couldn't insert into this chunk, size already wouldn't allow it
        } else {
            unsafe {
                let t = val.take().unwrap();

                // this is sound since because of the idx bump we can never
                // have an overlap in cells (no aliasing of actual cells) so long
                // as other guarantees are upheld for how we index into content
                let mchunk: *mut Chunk<T> = std::mem::transmute(chunk);

                (*mchunk).content[idx] = MaybeUninit::new(t);

                let aidx = AtomicVecIndex {
                    idx: idx as i32,
                    chunk: chunk_idx,
                    self_key: self.self_key,
                };

                //let r = chunk.content[idx].assume_init_ref();
                let r = (*mchunk).content[idx].assume_init_mut();

                Some((aidx, r))
            }
        }
    }

    unsafe fn try_insert_chunk_idx(
        &self,
        idx: usize,
        val: &mut Option<T>,
    ) -> Option<(AtomicVecIndex, &mut T)> {
        //println!("getting chunk at idx {}", idx);
        let chunk = self.get_chunk(idx);

        self.try_insert_chunk(idx as i32, unsafe { chunk.as_ref().unwrap() }, val)
    }

    /// Marked unsafe since the &mut T aliases with the
    /// slot that AtomicVecIndex could be used to access
    unsafe fn push_internal(&self, e: T) -> (AtomicVecIndex, &mut T) {
        // don't need to check every chunk, can just start with current
        // chunk
        let mut chunk = self.cur.load(std::sync::atomic::Ordering::Acquire);

        let mut val = Some(e);

        while chunk < self.chunks.len() {
            #[allow(unused_unsafe)]
            unsafe {
                match self.try_insert_chunk_idx(chunk, &mut val) {
                    Some(avi) => return avi,
                    None => chunk += 1,
                }
            }
        }

        panic!("Exceeded AVec size constraint");
    }

    /// Safety contract: f must not attempt to
    /// index into self using any operator (such as get) using the passed AtomicVecIndex
    ///
    /// This function should only be used for creating self referential structures,
    /// where some T needs to know at what index it resides.
    ///
    /// Indexing into self using the given AtomicVecIndex may allow creating an & alias
    /// to the T that was already passed as &mut T, do not do this!
    pub unsafe fn push_with(
        &self,
        e: T,
        f: impl FnOnce(&mut T, AtomicVecIndex),
    ) -> (AtomicVecIndex, &T) {
        #[allow(unused_unsafe)]
        unsafe {
            let (avi, r) = self.push_internal(e);

            f(r, avi);

            (avi, r)
        }
    }

    pub fn push(&self, e: T) -> (AtomicVecIndex, &T) {
        unsafe {
            let (avi, r) = self.push_internal(e);
            (avi, r as &T)
        }
    }

    pub fn get(&self, idx: AtomicVecIndex) -> &T {
        //println!("get was called for avec with idx {}", idx);
        if idx.self_key != self.self_key {
            panic!("Tried to use a token from another AVec on self, this could cause unsoundness!");
        } else {
            let chunk = self.get_chunk(idx.chunk as usize);

            let chunk = unsafe { chunk.as_ref().unwrap() };
            unsafe {
                let t = chunk.content[idx.idx as usize].assume_init_ref();
                t
            }
        }
    }

    /// The heart wants what the heart wants, I guess
    pub fn get_mut(&mut self, idx: AtomicVecIndex) -> &mut T {
        if idx.self_key != self.self_key {
            panic!("Tried to use a token from another AVec on self, this could cause unsoundness!");
        } else {
            let chunk = self.get_chunk(idx.chunk as usize);

            let chunk = unsafe { (chunk as *mut Chunk<T>).as_mut().unwrap() };
            unsafe {
                let t = chunk.content[idx.idx as usize].assume_init_mut();
                t
            }
        }
    }

    /// first return is the chunk to index into, second is the slot within chunk
    pub fn idx_to_pair(&self, idx: usize) -> (usize, usize) {
        // knapsack problem :)
        for i in (0..self.lengths.len()).rev() {
            let l = self.lengths[i];
            if l < idx {
                let chunk = i;
                let slot = idx - l;
                return (chunk, slot);
            }
        }

        panic!("bad idx given");
    }
}

#[derive(Debug, Copy, Clone, Hash, Ord, PartialEq, PartialOrd, Eq)]
pub struct AtomicVecIndex {
    // intentionally not public, should not be possible to construct this type externally
    chunk: i32,
    idx: i32,
    self_key: usize, // used to verify that this index came from the vec it is trying to index into
}

impl Display for AtomicVecIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Index({}, {})", self.chunk, self.idx)
    }
}
