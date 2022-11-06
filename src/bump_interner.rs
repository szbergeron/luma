use std::collections::HashMap;

use dashmap::DashMap;
use futures::lock::Mutex;

/// An SStr can be resolved in an interner without having to have it pinned
pub struct SStr {
    shard: u8, // we limit shards to 256
    bin: u8, // it's fine to limit this to 256 as they expand roughly exponentially
    span: u16, // we don't allow individual strings to be more than 16k
    start: u32, // bins will never be more than 4 gigs allocated
}

pub struct Bumper<const N: u8> {
    shards: [Shard; N],
}

struct Shard {
    members: Mutex<MemberMap>,
    backing: ContData,
}

struct MemberMap {
    buckets: Vec<>
}

/// An expanding bump allocator with data-acking growth
struct ContData<const N: usize> {
    bins: [Bin; N],
    current: 
}
