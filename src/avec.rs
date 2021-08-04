use std::{alloc, marker::PhantomData, mem::MaybeUninit, ptr::{addr_of_mut, null_mut}, sync::atomic::{AtomicPtr, AtomicUsize}};

//#[repr(C)]
struct Chunk<T> {
    size: usize,
    cur: AtomicUsize,
    content: [MaybeUninit<T>],
}

impl<T> Chunk<T> {
    /// Based on code provided by Plecra#5251 for fat ptr handling, declared public domain (MIT
    /// compatible)
    pub fn with_capacity(num: usize) -> *mut Self {
        let layout = unsafe {
            std::alloc::Layout::for_value_raw(
                std::ptr::slice_from_raw_parts(std::ptr::null::<()>(), num) as *const Self,
            )
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
        //according to https://doc.rust-lang.org/nightly/core/ptr/trait.Pointee.html, the
        //metadata of the last field is the metadata for the fat ptr used
        std::ptr::from_raw_parts_mut(r, len)
        /*let conv: *mut T = std::mem::transmute(r);
        let r: *mut [T] = core::slice::from_raw_parts_mut(conv, len);
        r as *mut Self*/
    }
}

#[repr(C)]
struct SizedChunk<T> {
    size: usize,
    cur: AtomicUsize,
    content: [MaybeUninit<T>; 1],
}

pub struct AtomicVec<T, const CHUNK_COUNT: usize, const FIRST_CHUNK_SIZE: usize> {
    self_key: usize,
    chunks: [AtomicPtr<()>; CHUNK_COUNT],
    lengths: [usize; CHUNK_COUNT],
    cur: AtomicUsize,
    phantom: PhantomData<T>,
}

impl<T, const CHUNK_COUNT: usize, const FIRST_CHUNK_SIZE: usize> AtomicVec<T, CHUNK_COUNT, FIRST_CHUNK_SIZE> {
    fn make_lengths<const COUNT: usize>() -> [usize; COUNT] {
        let mut c = [0; COUNT];

        let mut cur_size = FIRST_CHUNK_SIZE;

        for i in 0..c.len() {
            c[i] = cur_size;
            cur_size *= 2;
        }

        c
    }

    pub fn new() -> AtomicVec<T, 32, 32> {
        static init_key: AtomicUsize = AtomicUsize::new(0);
        //const nptr: AtomicPtr<()> = AtomicPtr::new(null_mut());

        /*let mut chunks = Vec::new();
        for i in 0..32 {
            chunks.push(AtomicPtr::new(null_mut()));
        }

        //let (slice, _, _) = chunks.into_raw_parts();*/

        AtomicVec {
            self_key: init_key.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            chunks: Default::default(), // null ptrs
            cur: AtomicUsize::new(0),
            phantom: Default::default(),
            lengths: Self::make_lengths(),
        }
    }

    fn get_chunk(&self, idx: usize) -> &Chunk<T> {
        // check first if it was already non-null
        let ptr = self.chunks[idx].load(std::sync::atomic::Ordering::Acquire);

        let r = if ptr.is_null() {
            // do alloc speculatively since it was null at first load
            let size = (2 as usize).pow(idx as u32) * FIRST_CHUNK_SIZE;

            let fat_ptr: *mut Chunk<T> = Chunk::with_capacity(size);

            //core::slice::
            let (ptr, _meta) = fat_ptr.to_raw_parts();

            //let pinned = Pin::new(Chunk::with_capacity(size));
            //

            let res = self
                .chunks[idx]
                // both are AcqRel because I don't have the mental capacity to figure out if
                // anything else is safe right now. Doesn't need SeqCst since threads only care
                // about their own load/store so no "3rd" thread is involved in any interaction
                // here
                .compare_exchange(null_mut(), ptr, std::sync::atomic::Ordering::AcqRel, std::sync::atomic::Ordering::AcqRel);

            match res {
                Ok(p) => {
                    // we got a chance to write our ptr, no other thread did it faster
                    p
                },
                Err(old) => {
                    // we need to dealloc ourself since some other thread already wrote the value
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

        unsafe {
            Chunk::make_fat_ptr(r, self.lengths[idx]).as_ref().unwrap()
        }
    }

    fn try_insert_chunk<'c>(
        &self,
        chunk_idx: i32,
        chunk: &'c Chunk<T>,
        val: &mut Option<T>,
    ) -> Option<(AtomicVecIndex, &'c T)> {
        let idx = chunk.cur.fetch_add(1, std::sync::atomic::Ordering::AcqRel); // bump idx speculatively

        if idx >= chunk.size {
            None // couldn't insert into this chunk, size already wouldn't allow it
        } else {
            unsafe {
                let t = val.take().unwrap();

                // this is sound since because of the idx bump we can never
                // have an overlap in cells (no aliasing of actual cells) so long
                // as other guarantees are upheld for how we index into content
                let mchunk: &'c mut Chunk<T> = std::mem::transmute(chunk);

                mchunk.content[idx] = MaybeUninit::new(t);

                let aidx = AtomicVecIndex {
                    idx: idx as i32,
                    chunk: chunk_idx,
                    self_key: self.self_key,
                };
                let r = chunk.content[idx].assume_init_ref();

                Some((aidx, r))
            }
        }
    }

    unsafe fn try_insert_chunk_idx(
        &self,
        idx: usize,
        val: &mut Option<T>,
    ) -> Option<(AtomicVecIndex, &T)> {
        let chunk = self.get_chunk(idx);

        self.try_insert_chunk(idx as i32, chunk, val)
    }

    pub fn push(&self, e: T) -> (AtomicVecIndex, &T) {
        // don't need to check every chunk, can just start with current
        // chunk
        let mut chunk = self.cur.load(std::sync::atomic::Ordering::Acquire);

        let mut val = Some(e);

        while chunk < CHUNK_COUNT {
            unsafe {
                match self.try_insert_chunk_idx(chunk, &mut val) {
                    Some(avi) => return avi,
                    None => chunk += 1,
                }
            }
        }

        panic!("Exceeded AVec size constraint");
    }

    pub fn get(&self, idx: AtomicVecIndex) -> &T {
        if idx.self_key != self.self_key {
            panic!("Tried to use a token from another AVec on self, this could cause unsoundness!");
        } else {
            let chunk = self.get_chunk(idx.chunk as usize);
            unsafe {
                let t = chunk.content[idx.idx as usize].assume_init_ref();
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

pub struct AtomicVecIndex {
    // intentionally not public, should not be possible to construct this type externally
    chunk: i32,
    idx: i32,
    self_key: usize, // used to verify that this index came from the vec it is trying to index into
}
