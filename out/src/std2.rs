use std::{
    borrow::BorrowMut,
    cell::{RefCell, RefMut, UnsafeCell},
    cmp::Ordering,
    collections::HashMap,
    pin::Pin,
    sync::{atomic::AtomicU16, Arc, Mutex},
    thread::panicking,
    time::{Duration, Instant},
};

use rclite::Rc;

use rand::{rngs::SmallRng, Rng, SeedableRng};

const IS_DYN: bool = true;

pub struct LumaVec<T> {
    inner: Arc<Mutex<Vec<T>>>,
}

pub fn luma_print_fast<T: std::fmt::Debug>(v: T) {
    println!("{v:?}");
}

pub fn luma_print_slow(v: Value) -> Value {
    println!("{v}");

    Value::Uninhabited(NONE_OBJ.clone())
}

pub fn rand_of_len_slow(len: Value) -> Value {
    match len {
        Value::I64(v, _) => Value::String(rand_of_len_fast(v), STRING_OBJ.clone()),
        v => unreachable!("can't make len from val of type {v:?}"),
    }
}

pub fn rand_of_len_fast(len: i64) -> String {
    let mut base = String::new();
    for i in 0..len {
        let c = ((rand_i64() % 26) + 97) as i8;
        //let c = c.abs; // make sure 0th bit is clear
        let c = c as u8 & 0x7F;

        base.push(c as char);
    }

    base
}

//static RNG: UnsafeCell<
pub fn luma_print_dur(dur: Duration, m: String) {
    /*println!(
        "{m}: Took {:0>1}s.{:0>3}.{}",
        dur.as_secs(),
        dur.subsec_millis(),
        dur.subsec_micros()
    );*/
    //println!("{m}{}.{}", dur.as_secs(), dur.subsec_millis());
    println!("{m}{}", dur.as_secs_f64());
}

pub fn luma_to_string_fast<T: std::fmt::Display>(v: T) -> String {
    format!("{v}")
}

pub fn rand_i64() -> i64 {
    let mut r = rand::thread_rng();
    let mut sng = SmallRng::from_rng(&mut r).unwrap();
    sng.gen()
}

pub fn rand_f64() -> f64 {
    let mut r = rand::thread_rng();
    let mut sng = SmallRng::from_rng(&mut r).unwrap();
    sng.gen()
}

pub fn panic_with<T: std::fmt::Debug>(v: T) -> ! {
    panic!("panic: {v:?}")
}

/// Contract: the return of this is only valid for as long as no other
/// get_field call is made on this object, as field location is not stable
/// across hashmap resizes
#[inline(never)]
pub fn __luma_get_field(object: *mut Value, field: String) -> *mut Value {
    //println!("Getting field {field} on {:?}", unsafe { &*object });
    if !IS_DYN {
        panic!("wrong get_field for kind of build");
    }

    unsafe {
        match &*object {
            Value::Ref(r) => __luma_get_field(*r, field),
            Value::Object(v) => {
                let r = v
                    .inner
                    .get()
                    .as_mut()
                    .unwrap()
                    .fields
                    .entry(field)
                    .or_insert_with(|| {
                        //println!("field didn't exist yet, so making an empty one");
                        Value::Uninhabited(NONE_OBJ.clone())
                    });

                r
            }
            Value::I64(p, o) => __luma_get_field(&mut Value::Object(o.clone()), field),
            Value::F64(p, o) => __luma_get_field(&mut Value::Object(o.clone()), field),
            Value::String(p, o) => __luma_get_field(&mut Value::Object(o.clone()), field),
            Value::Vec(p, o) => __luma_get_field(&mut Value::Object(o.clone()), field),
            Value::Instant(p, o) => __luma_get_field(&mut Value::Object(o.clone()), field),
            Value::char(p, o) => __luma_get_field(&mut Value::Object(o.clone()), field),
            Value::Bool(p, o) => __luma_get_field(&mut Value::Object(o.clone()), field),
            Value::Duration(p, o) => __luma_get_field(&mut Value::Object(o.clone()), field),
            Value::Some(p, o) => __luma_get_field(&mut Value::Object(o.clone()), field),
            Value::Uninhabited(o) => __luma_get_field(&mut Value::Object(o.clone()), field),
            other => todo!("handle value {other:?}"),
        }
    }
}

#[inline(never)]
pub fn __luma_as_callable(object: &Value) -> *const fn() {
    match object {
        Value::Callable(c) => {
            let v = *c;

            //println!("fn is {v:?}");

            v
        }
        Value::Ref(r) => unsafe { __luma_as_callable(r.as_mut().unwrap()) },
        o => panic!("{o:?} is not a callable"),
    }
}

pub type ObjectHandle = Arc<DynamicObjectWrapper>;

pub struct DynamicObjectWrapper {
    pub inner: UnsafeCell<DynamicObject>,
}

unsafe impl Send for DynamicObjectWrapper {}
unsafe impl Sync for DynamicObjectWrapper {}

pub type luma_unit = ();

pub trait AsVariable: Clone {
    fn assign_from(&mut self, v: Self);
}

/*
impl<T> AsVariable for T where T: Clone {
    fn assign_from(&mut self, v: Self) {
        *self = v;
    }
}

impl<T> AsVariable for FastRefHandle<T> {
    fn assign_from(&mut self, v: Self) {
        if let Some(i) = &self.inner {
        }
    }
}
*/

pub struct FastHandleTarget<T> {
    //mut_borrows: AtomicU16,
    //immut_borrows: AtomicU16,
    inner: UnsafeCell<T>,
}

impl<T> FastHandleTarget<T> {
    /*pub fn new_mut_handle(&self) -> &mut T {
        assert!(self.mut_borrows.fetch_add(1, std::sync::atomic::Ordering::Acquire) == 0);
        unsafe { self.inner.get().as_mut().unwrap() }
    }

    pub fn new_immut_handle(&self) -> &mut T {
        self.immut_borrows.fetch_add(1, std::sync::atomic::Ordering::Acquire);
        assert!(self.mut_borrows.load(std::sync::atomic::Ordering::Acquire) == 0);

        unsafe { self.inner.get().as_mut().unwrap() }
    }

    pub fn drop_immut(&self) {
        self.immut_borrows.fetch_sub(1, std::sync::atomic::Ordering::Release);
    }

    pub fn drop_mut(&self) {
        self.mut_borrows.fetch_sub(1, std::sync::atomic::Ordering::Release);
    }*/

    pub fn new(v: T) -> Self {
        //Self { mut_borrows: AtomicU16::new(0), immut_borrows: AtomicU16::new(0), inner: UnsafeCell::new(v) }
        Self {
            inner: UnsafeCell::new(v),
        }
    }
}

pub struct FastRefHandle<T> {
    pub inner: Pin<Rc<FastHandleTarget<T>>>,
}

pub struct FastValHandle<T> {
    inner: T,
}

impl<T> std::default::Default for FastValHandle<T>
where
    T: std::default::Default,
{
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<T> std::default::Default for FastRefHandle<T>
where
    T: std::default::Default,
{
    fn default() -> Self {
        Self {
            inner: Rc::pin(FastHandleTarget::new(T::default())),
        }
    }
}

impl<T> std::fmt::Debug for FastRefHandle<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.borrow_mut().fmt(f)
    }
}

/*
pub trait FastValue<T> {
    pub fn as_lval(self) -> FastRVal<T>;
}

pub enum FastLVal<T> {
    Handle(FastRefHandle<T>),
    Value(FastValHandle<T>),
}

pub struct FastRVal<T> {
    //originally:
}*/

//impl<T> std::ops::Deref for FastRVal<T> {}

/*impl<T> std::ops::Deref for FastRefHandle<T> {
    type Target = Ref<T>;

    fn deref(&self) -> &Self::Target {
        //assert!(self.
        self.inner.as_ref().unwrap().deref()
    }
}*/

/*impl<T> std::ops::DerefMut for FastRefHandle<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self
    }
}*/

impl<T> FastRefHandle<T> {
    pub fn from_val(v: T) -> Self {
        Self {
            inner: Rc::pin(FastHandleTarget::new(v)),
        }
    }

    //pub fn as_ptr(&self)

    pub fn borrow_mut(&self) -> &mut T {
        unsafe {
            self.inner
                .as_ref()
                //.expect("tried to read from a None variable or ref field")
                .inner
                .get()
                .as_mut()
                .unwrap()
        }
    }

    /*
    pub fn borrow(&self) -> Ref<T> {
        self.inner
            .as_ref()
            .expect("tried to read from None variable or ref field")
            .inner
            .borrow()
    }*/
}

impl<T> Clone for FastRefHandle<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Object(ObjectHandle),
    I32(i32, ObjectHandle),
    I64(i64, ObjectHandle),
    F64(f64, ObjectHandle),
    Bool(bool, ObjectHandle),
    Instant(Instant, ObjectHandle),
    Duration(Duration, ObjectHandle),
    String(String, ObjectHandle),
    char(char, ObjectHandle),
    Ref(*mut Value),

    Vec(Rc<UnsafeCell<Vec<Value>>>, ObjectHandle),

    Callable(*const fn()),

    /// Basically means None
    Uninhabited(ObjectHandle),
    Some(Box<Value>, ObjectHandle),
}

unsafe impl Sync for Value {}
unsafe impl Send for Value {}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Object(o) => write!(f, "Object({})", unsafe {
                o.inner.get().as_ref().unwrap().object_tid
            }),
            Value::Vec(v, _) => writeln!(f, "vec({v:?})"),
            Value::I32(v, _) => write!(f, "i32({v})"),
            Value::I64(v, _) => write!(f, "i64({v})"),
            Value::F64(v, _) => write!(f, "f64({v})"),
            Value::Bool(v, _) => write!(f, "bool({v})"),
            Value::String(v, _) => write!(f, "str({v})"),
            r @ Value::Ref(_) => write!(f, "ref({:?})", r.root()),
            Value::Callable(c) => write!(f, "callable()"),
            Value::Uninhabited(_) => write!(f, "None"),
            Value::Some(v, _) => write!(f, "Some({v:?}"),
            Value::Duration(v, _) => write!(f, "{}", v.as_secs_f64()),
            Value::Instant(v, _) => write!(
                f,
                "Instant({} before now)",
                (Instant::now() - *v).as_secs_f64()
            ),
            Value::char(v, _) => write!(f, "char({})", v),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Object(o) => write!(f, "Object({})", unsafe {
                o.inner.get().as_ref().unwrap().object_tid
            }),
            Value::Vec(v, _) => {
                //writeln!(f, "[{v}]"),
                write!(f, "[")?;
                for v in unsafe { v.get().as_ref().unwrap().iter() } {
                    write!(f, "{v}")?;
                }

                write!(f, "]")
            }
            Value::I32(v, _) => write!(f, "{v}"),
            Value::I64(v, _) => write!(f, "{v}"),
            Value::F64(v, _) => write!(f, "{v}"),
            Value::Bool(v, _) => write!(f, "{v}"),
            Value::String(v, _) => write!(f, "{v}"),
            r @ Value::Ref(_) => write!(f, "{:?}", r.root()),
            Value::Callable(c) => write!(f, "callable({c:?})"),
            Value::Duration(v, _) => write!(f, "{}", v.as_secs_f64()),
            Value::Instant(v, _) => write!(f, "{} efore now", (Instant::now() - *v).as_secs_f64()),
            Value::char(v, _) => write!(f, "{}", v),
            Value::Uninhabited(_) => write!(f, "None"),
            Value::Some(v, _) => write!(f, "Some({v:?}"),
        }
    }
}

impl Value {
    #[inline(never)]
    pub fn is_true(&self) -> bool {
        match self {
            Self::Bool(b, o) => *b,
            _other => panic!("tried to take truthiness of non-bool"),
        }
    }

    pub fn from_fn(f: *const fn()) -> Value {
        Self::Callable(f)
    }

    pub fn root(&self) -> &Value {
        match self {
            Self::Ref(v) => {
                let p = *v;
                unsafe { &*p }.root()
            }
            other => other,
        }
    }

    pub fn root_mut(&mut self) -> &mut Value {
        todo!()
    }
}

/// Marker type, should never be instantiated
/// Also doesn't impl drop,
pub struct Object {}

impl std::ops::Drop for Object {
    fn drop(&mut self) {
        // noop
    }
}

#[allow(dead_code)]
pub struct DynamicObject {
    object_tid: u64,
    fields: HashMap<String, Value>,
}

impl DynamicObject {
    #[inline(never)]
    pub fn new(tid: u64) -> Value {
        Value::Object(Self::new_as_handle(tid))
    }

    #[inline(never)]
    pub fn new_as_handle(tid: u64) -> ObjectHandle {
        Arc::new(DynamicObjectWrapper {
            inner: UnsafeCell::new(DynamicObject {
                object_tid: tid,
                fields: HashMap::new(),
            }),
        })
    }
}

#[inline(never)]
pub fn __luma_assign(from: *mut Value, into: *mut Value) {
    unsafe {
        match &*into {
            Value::Ref(r) => __luma_assign(from, *r),
            _ => {
                match &*from {
                    Value::Object(o) => {
                        if IS_DYN {
                            let _d_s: &DynamicObject = std::mem::transmute(o.as_ref());
                            // fill this in later to verify object types
                            //let d_d: &DynamicObject = std::mem::transmute(src)
                        }
                        *into = Value::Object(o.clone());
                    }
                    Value::String(v, o) => {
                        *into = Value::String(v.clone(), o.clone());
                    }
                    Value::Uninhabited(v) => {
                        *into = Value::Uninhabited(v.clone());
                    }
                    Value::Ref(r) => __luma_assign(*r, into),
                    Value::Callable(c) => {
                        *into = Value::Callable(*c);
                    }
                    _other_byval => {
                        *into = _other_byval.clone();
                    }
                }
            }
        }
    }
}

pub fn luma_slow_bool_compare_inner(a: Value, b: Value) -> Option<Ordering> {
    match (a.root(), b.root()) {
        (Value::Bool(a, _), Value::Bool(b, _)) => Some(a.cmp(b)),
        (Value::I32(a, _), Value::I32(b, _)) => Some(a.cmp(b)),
        (Value::I64(a, _), Value::I64(b, _)) => Some(a.cmp(b)),
        _ => None,
    }
}

pub fn new_none() -> Value {
    Value::Uninhabited(NONE_OBJ.clone())
}

pub fn new_some(v: Value) -> Value {
    Value::Some(Box::new(v), SOME_OBJ.clone())
}

/*pub fn slow_build_bool_obj() -> ObjectHandle {
    let d = DynamicObject::new_as_handle(u64::MAX - 1);

    todo!()
}*/
pub fn pass_through(v: Value) -> Value {
    v
}

pub fn luma_obj_true(v: Value) -> Value {
    Value::Bool(true, BOOL_OBJ.clone())
}

pub fn luma_obj_false(v: Value) -> Value {
    Value::Bool(false, BOOL_OBJ.clone())
}

lazy_static::lazy_static! {
    pub static ref BOOL_OBJ: ObjectHandle = {
        let h = DynamicObject::new_as_handle(u64::MAX - 1);

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        f.fields.insert("operator[_==_]".to_owned(), Value::from_fn(luma_op_eq_slow as *const fn()));
        f.fields.insert("operator[_!=_]".to_owned(), Value::from_fn(luma_op_ne_slow as *const fn()));
        f.fields.insert("operator[!_]".to_owned(), Value::from_fn(luma_op_not_slow as *const fn()));
        f.fields.insert("to_string".to_owned(), Value::from_fn(luma_to_string_slow as *const fn()));

        h
    };

    pub static ref INT_OBJ: ObjectHandle = {
        let h = DynamicObject::new_as_handle(u64::MAX - 2);

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        f.fields.insert("operator[_==_]".to_owned(), Value::from_fn(luma_op_eq_slow as *const fn()));
        f.fields.insert("operator[_!=_]".to_owned(), Value::from_fn(luma_op_ne_slow as *const fn()));
        f.fields.insert("operator[_+_]".to_owned(), Value::from_fn(luma_op_add_slow as *const fn()));
        f.fields.insert("operator[_-_]".to_owned(), Value::from_fn(luma_op_subtract_slow as *const fn()));
        f.fields.insert("operator[_*_]".to_owned(), Value::from_fn(luma_op_mul_slow as *const fn()));
        f.fields.insert("operator[_<_]".to_owned(), Value::from_fn(luma_op_lt_slow as *const fn()));
        f.fields.insert("operator[_>_]".to_owned(), Value::from_fn(luma_op_gt_slow as *const fn()));
        f.fields.insert("to_string".to_owned(), Value::from_fn(luma_to_string_slow as *const fn()));
        f.fields.insert("modulo".to_owned(), Value::from_fn(luma_op_modulo_slow as *const fn()));
        f.fields.insert("abs".to_owned(), Value::from_fn(luma_op_abs_slow as *const fn()));
        f.fields.insert("sqrt".to_owned(), Value::from_fn(luma_op_sqrt_slow as *const fn()));
        f.fields.insert("pow".to_owned(), Value::from_fn(luma_op_pow_slow as *const fn()));

        h
    };

    pub static ref CHAR_OBJ: ObjectHandle = {
        let h = DynamicObject::new_as_handle(u64::MAX - 3);

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        f.fields.insert("as_int".to_owned(), Value::from_fn(luma_char_op_as_int as *const fn()));

        h
    };

    pub static ref STRING_OBJ: ObjectHandle = {
        let h = DynamicObject::new_as_handle(u64::MAX - 3);

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        f.fields.insert("operator[_+_]".to_owned(), Value::from_fn(luma_op_add_slow as *const fn()));

        h
    };

    pub static ref DURATION_OBJ: ObjectHandle = {
        let h = DynamicObject::new_as_handle(u64::MAX - 3);

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        f.fields.insert("print".to_owned(), Value::from_fn(luma_print_slow as *const fn()));

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        h
    };

    pub static ref INSTANT_OBJ: ObjectHandle = {
        let h = DynamicObject::new_as_handle(u64::MAX - 3);

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        f.fields.insert("operator[_-_]".to_owned(), Value::from_fn(luma_op_subtract_slow as *const fn()));

        h
    };

    pub static ref VEC_OBJ: ObjectHandle = {
        let h = DynamicObject::new_as_handle(u64::MAX - 4);

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        pub fn luma_vec_push_slow(v: Value, i: Value) -> Value {
            let root = v.root();

            match root {
                Value::Vec(v, o) => {
                    unsafe { v.get().as_mut().unwrap().push(i) };

                    Value::Uninhabited(NONE_OBJ.clone())
                }
                _ => todo!(),
            }
        }

        pub fn luma_vec_get_slow(v: Value, ind: Value) -> Value {
            let root = v.root();

            let ind = match ind.root() {
                Value::I64(v, _) => *v,
                other => panic!("can't index by {other:?}"),
            };

            match root {
                Value::Vec(v, o) => unsafe {
                    v.get()
                        .as_mut()
                        .unwrap()
                        .get(ind as usize)
                        .cloned()
                        .unwrap()
                },
                _ => todo!(),
            }
        }

        pub fn luma_vec_set_slow(v: Value, ind: Value, val: Value) -> Value {
            let root = v.root();

            let ind = match ind.root() {
                Value::I64(v, _) => *v,
                other => panic!("can't index by {other:?}"),
            };

            match root {
                Value::Vec(v, o) => unsafe {
                    v.get().as_mut().unwrap()[ind as usize] = val;

                    Value::Uninhabited(NONE_OBJ.clone())
                },
                _ => todo!(),
            }
        }

        pub fn pop_back(v: Value) -> Value {
            let r = v.root();
            let rv = as_vec(r);

            //rv.pop().unwrap()
            rv.pop().map(|v| new_some(v)).unwrap_or_else(|| new_none())
        }

        pub fn as_vec(v: &Value) -> &mut Vec<Value> {

            match v {
                Value::Vec(v, _) => unsafe { v.get().as_mut().unwrap() },
                other => panic!("tried to interpret non-vec {other:?} as vec")
            }
        }

        f.fields.insert("push".to_owned(), Value::from_fn(luma_vec_push_slow as *const fn()));
        f.fields.insert("get".to_owned(), Value::from_fn(luma_vec_get_slow as *const fn()));
        f.fields.insert("set".to_owned(), Value::from_fn(luma_vec_set_slow as *const fn()));
        f.fields.insert("len".to_owned(), Value::from_fn(luma_vec_len_slow as *const fn()));
        f.fields.insert("to_string".to_owned(), Value::from_fn(luma_to_string_slow as *const fn()));
        f.fields.insert("pop_back".to_owned(), Value::from_fn(pop_back as *const fn()));

        h
    };

    pub static ref NONE_OBJ: ObjectHandle = {
        let h = DynamicObject::new_as_handle(u64::MAX - 4);

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        fn unwrap_none(v: Value) -> Value {
            panic!("called unwrap on a None value")
        }

        f.fields.insert("is_some".to_owned(), Value::from_fn(luma_obj_false as *const fn()));
        f.fields.insert("is_none".to_owned(), Value::from_fn(luma_obj_true as *const fn()));
        f.fields.insert("unwrap".to_owned(), Value::from_fn(unwrap_none as *const fn()));

        h
    };

    pub static ref SOME_OBJ: ObjectHandle = {
        fn unwrap_some(v: Value) -> Value {
            let r = v.root().clone();

            match r {
                Value::Some(v, _) => *v,
                _ => panic!("unwrapped something not a Some")
            }
        }

        let h = DynamicObject::new_as_handle(u64::MAX - 4);

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        f.fields.insert("is_some".to_owned(), Value::from_fn(luma_obj_true as *const fn()));
        f.fields.insert("is_none".to_owned(), Value::from_fn(luma_obj_false as *const fn()));
        f.fields.insert("unwrap".to_owned(), Value::from_fn(unwrap_some as *const fn()));

        h
    };


    pub static ref FLOAT_OBJ: ObjectHandle = {
        let h = DynamicObject::new_as_handle(u64::MAX - 5);

        let mut f = unsafe { h.inner.get().as_mut().unwrap() };

        f.fields.insert("operator[_==_]".to_owned(), Value::from_fn(luma_op_eq_slow as *const fn()));
        f.fields.insert("operator[_!=_]".to_owned(), Value::from_fn(luma_op_ne_slow as *const fn()));
        f.fields.insert("operator[_+_]".to_owned(), Value::from_fn(luma_op_add_slow as *const fn()));
        f.fields.insert("operator[_-_]".to_owned(), Value::from_fn(luma_op_subtract_slow as *const fn()));
        f.fields.insert("operator[_*_]".to_owned(), Value::from_fn(luma_op_mul_slow as *const fn()));
        f.fields.insert("operator[_/_]".to_owned(), Value::from_fn(luma_op_divide_slow as *const fn()));
        f.fields.insert("operator[_%_]".to_owned(), Value::from_fn(luma_op_modulo_slow as *const fn()));
        f.fields.insert("operator[_<_]".to_owned(), Value::from_fn(luma_op_lt_slow as *const fn()));
        f.fields.insert("operator[_>_]".to_owned(), Value::from_fn(luma_op_gt_slow as *const fn()));
        f.fields.insert("operator[_>=_]".to_owned(), Value::from_fn(luma_op_gte_slow as *const fn()));
        f.fields.insert("modulo".to_owned(), Value::from_fn(luma_op_modulo_slow as *const fn()));
        f.fields.insert("abs".to_owned(), Value::from_fn(luma_op_abs_slow as *const fn()));
        f.fields.insert("sqrt".to_owned(), Value::from_fn(luma_op_sqrt_slow as *const fn()));
        f.fields.insert("pow".to_owned(), Value::from_fn(luma_op_pow_slow as *const fn()));
        f.fields.insert("to_string".to_owned(), Value::from_fn(luma_to_string_slow as *const fn()));

        h
    };
}

/*
pub fn luma_obj_is_some_slow(v: Value) -> Value {
    match v {
        Value::Uninhabited(_) => Value::Bool(false, BOOL_OBJ.clone()),
        _ => Value::Bool(true, BOOL_OBJ.clone()),
    }
}

pub fn luma_obj_is_none_slow(v: Value) -> Value {
    match v {
        Value::Uninhabited(_) => Value::Bool(true, BOOL_OBJ.clone()),
        _ => Value::Bool(false, BOOL_OBJ.clone()),
    }
}
*/

pub fn luma_slow_new_bool(v: bool) -> Value {
    Value::Bool(v, BOOL_OBJ.clone())
}

pub fn luma_slow_new_i64(v: i64) -> Value {
    Value::I64(v, INT_OBJ.clone())
}

pub fn luma_slow_new_f64(v: f64) -> Value {
    Value::F64(v, FLOAT_OBJ.clone())
}

pub fn luma_slow_new_string(s: &str) -> Value {
    Value::String(s.to_owned(), STRING_OBJ.clone())
}

pub fn luma_vec_new_slow() -> Value {
    Value::Vec(Rc::new(UnsafeCell::new(Vec::new())), VEC_OBJ.clone())
}

pub fn luma_instant_new_slow() -> Value {
    Value::Instant(Instant::now(), INSTANT_OBJ.clone())
}

#[inline(never)]
pub fn luma_blackhole<T: std::fmt::Debug>(v: T) -> i64 {
    let as_fmt = format!("{v:?}");
    let mut s = 0i64;
    /*let p = (&v as *const T) as *const u8;

    for byte in 0..std::mem::size_of::<T>() {
        let b = *p;
        let bb = b as i64;

        s += bb;
    }*/

    for c in as_fmt.chars() {
        let ai = c as i64;

        s += ai;
    }

    s
}

pub fn luma_op_eq_slow(a: Value, b: Value) -> Value {
    luma_slow_new_bool(match luma_slow_bool_compare_inner(a, b) {
        Some(Ordering::Equal) => true,
        _ => false,
    })
}

pub fn luma_op_ne_slow(a: Value, b: Value) -> Value {
    luma_slow_new_bool(match luma_slow_bool_compare_inner(a, b) {
        Some(Ordering::Equal) => false,
        _ => true,
    })
}

pub fn luma_op_not_slow(a: Value) -> Value {
    match a {
        Value::Bool(b, bo) => Value::Bool(!b, bo),
        _ => unreachable!(),
    }
}

pub fn luma_op_lt_slow(a: Value, b: Value) -> Value {
    luma_slow_new_bool(match luma_slow_bool_compare_inner(a, b) {
        Some(Ordering::Less) => true,
        _ => false,
    })
}

pub fn luma_op_gt_slow(a: Value, b: Value) -> Value {
    luma_slow_new_bool(match luma_slow_bool_compare_inner(a, b) {
        Some(Ordering::Greater) => true,
        _ => false,
    })
}

pub fn luma_op_gte_slow(a: Value, b: Value) -> Value {
    luma_slow_new_bool(match luma_slow_bool_compare_inner(a, b) {
        Some(Ordering::Greater) | Some(Ordering::Equal) => true,
        _ => false,
    })
}

/*pub fn luma_op_eq_(a: Value, b: Value) -> Value {
    luma_slow_new_bool(match luma__slow_bool_compare_inner(a, b) {
        Some(Ordering::Equal) => false,
        Some(_) => true,
        None => panic!("why"),
    })
}*/

pub fn luma_op_add_slow(a: Value, b: Value) -> Value {
    match (a.root(), b.root()) {
        (Value::I64(a, am), Value::I64(b, bm)) => {
            //println!("adding {a} and {b}");
            Value::I64(a + b, am.clone())
        }
        (Value::F64(a, am), Value::F64(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::F64(a + b, am.clone())
        }
        (Value::String(a, am), Value::String(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::String(format!("{a}{b}"), am.clone())
        }
        _ => unreachable!(),
    }
}

pub fn luma_op_subtract_slow(a: Value, b: Value) -> Value {
    match (a.root(), b.root()) {
        (Value::I64(a, am), Value::I64(b, bm)) => {
            //println!("subtracting {a} and {b}");
            Value::I64(a - b, am.clone())
        }
        (Value::F64(a, am), Value::F64(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::F64(a - b, am.clone())
        }
        (Value::Instant(a, am), Value::Instant(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::Duration(*a - *b, DURATION_OBJ.clone())
        }
        _ => unreachable!(),
    }
}

pub fn luma_op_mul_slow(a: Value, b: Value) -> Value {
    match (a.root(), b.root()) {
        (Value::I64(a, am), Value::I64(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::I64(a * b, am.clone())
        }
        (Value::F64(a, am), Value::F64(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::F64(a * b, am.clone())
        }
        _ => unreachable!(),
    }
}

pub fn luma_op_divide_slow(a: Value, b: Value) -> Value {
    match (a.root(), b.root()) {
        (Value::I64(a, am), Value::I64(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::I64(a / b, am.clone())
        }
        (Value::F64(a, am), Value::F64(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::F64(a / b, am.clone())
        }
        _ => unreachable!(),
    }
}

pub fn luma_op_modulo_slow(a: Value, b: Value) -> Value {
    match (a.root(), b.root()) {
        (Value::I64(a, am), Value::I64(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::I64(a % b, am.clone())
        }
        (Value::F64(a, am), Value::F64(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::F64(a % b, am.clone())
        }
        _ => unreachable!(),
    }
}

pub fn luma_op_pow_slow(a: Value, b: Value) -> Value {
    match (a.root(), b.root()) {
        (Value::I64(a, am), Value::I64(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::I64(a.pow(*b as u32), am.clone())
        }
        (Value::F64(a, am), Value::F64(b, bm)) => {
            //println!("multiplying {a} and {b}");
            Value::F64(a.powf(*b), am.clone())
        }
        _ => unreachable!(),
    }
}

pub fn luma_op_cos_slow(a: Value) -> Value {
    match a.root() {
        Value::F64(a, am) => {
            //println!("multiplying {a} and {b}");
            Value::F64(a.cos(), am.clone())
        }
        _ => unreachable!(),
    }
}

pub fn luma_op_abs_slow(a: Value) -> Value {
    match a.root() {
        Value::F64(a, am) => {
            //println!("multiplying {a} and {b}");
            Value::F64(a.abs(), am.clone())
        }
        Value::I64(a, am) => {
            //println!("multiplying {a} and {b}");
            Value::I64(a.abs(), am.clone())
        }
        _ => unreachable!(),
    }
}

pub fn luma_op_sqrt_slow(a: Value) -> Value {
    match a.root() {
        Value::F64(a, am) => {
            //println!("multiplying {a} and {b}");
            Value::F64(a.sqrt(), am.clone())
        }
        _ => unreachable!(),
    }
}

pub fn luma_f64_rand_slow() -> Value {
    Value::F64(rand_f64(), FLOAT_OBJ.clone())
}
pub fn luma_i64_rand_slow() -> Value {
    Value::I64(rand_i64(), FLOAT_OBJ.clone())
}

pub fn luma_str_op_head(v: Value) -> Value {
    match v {
        Value::String(s, _) => Value::char(s.chars().next().unwrap(), CHAR_OBJ.clone()),
        _ => unreachable!(),
    }
}

pub fn luma_str_op_tail(v: Value) -> Value {
    match v {
        Value::String(s, _) => Value::String(s[1..].to_owned(), STRING_OBJ.clone()),
        _ => unreachable!(),
    }
}

pub fn luma_str_op_len(v: Value) -> Value {
    match v {
        Value::String(s, _) => Value::I64(s.len() as i64, INT_OBJ.clone()),
        _ => unreachable!(),
    }
}

pub fn luma_char_op_as_int(v: Value) -> Value {
    match v {
        Value::char(c, _) => Value::I64(c as i64, INT_OBJ.clone()),
        _ => unreachable!(),
    }
}

pub fn luma_vec_len_slow(a: Value) -> Value {
    match a.root() {
        Value::Vec(a, am) => {
            //println!("multiplying {a} and {b}");
            Value::I64(
                unsafe { a.get().as_mut().unwrap().len() as i64 },
                am.clone(),
            )
        }
        _ => unreachable!(),
    }
}

pub fn luma_to_string_slow(a: Value) -> Value {
    let sv = format!("{a}");

    Value::String(sv, STRING_OBJ.clone())
}

pub fn luma_i64_op_mul_fast(a: i64, b: i64) -> i64 {
    a * b
}

pub fn luma_i64_op_divide_fast(a: i64, b: i64) -> i64 {
    a / b
}

pub fn luma_i64_op_subtract_fast(a: i64, b: i64) -> i64 {
    a - b
}

pub fn luma_i64_op_add_fast(a: i64, b: i64) -> i64 {
    a + b
}

pub fn luma_i64_op_eq_fast(a: i64, b: i64) -> bool {
    a == b
}

pub fn luma_i64_op_lt_fast(a: i64, b: i64) -> bool {
    a < b
}

pub fn luma_i64_op_gt_fast(a: i64, b: i64) -> bool {
    a > b
}

pub fn luma__fast_bool_compare_eq(a: bool, b: bool) -> bool {
    a == b
}

//pub fn luma__slow_cmp(a: Value, b: Value, with: FnOnce(&dy
