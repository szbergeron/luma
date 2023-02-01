use std::{assert_matches::debug_assert_matches, fmt::Debug, io::Write};

use crate::{
    helper::{interner::{IStr, SpurHelper}, CopyMethod},
    lowered::LoweredType,
};
use futures::never::Never;
use smallstr::SmallString;
use smallvec::SmallVec;

pub type LLVMBlob<const INLINE_INSTRUCTIONS: usize> = (LLVMChunk<INLINE_INSTRUCTIONS>, LLVMArg);

use modular_bitfield::prelude::*;

#[derive(Copy, Clone)]
pub struct LoweredTypeID(u32);

#[derive(Clone)]
pub struct Instruction<const INLINE: usize = 3> {
    inst: BumpStr,
    args: SmallVec<[LLVMArg; INLINE]>,
    // TODO: instruction metadata, bitfield?
    //output: Option<LLVMVar>,
}

impl LoweredTypeID {
    pub fn resolve(self, within: &Option<Never>) -> &LoweredType {
        todo!()
    }
}

//struct LLVMValue {}

pub type LLVMIdent = SmallString<[u8; 8]>;

/// Returns a constantly sized array of ascii chars
/// to be used as an identifier
trait ToLLVMIdentifier<const N: usize> {
    fn to_ident(self) -> SmallString<[u8; N]>;
}

impl ToLLVMIdentifier<8> for u32 {
    fn to_ident(self) -> SmallString<[u8; 8]> {
        let bytes: [u8; 4] = self.to_le_bytes();

        let mut out = SmallString::new();

        for (chunk, byte) in bytes.iter().enumerate() {
            let high = ((byte >> 4) & 0xf) + ('a' as u8);
            let low = (byte & 0xf) + ('a' as u8);

            out.push(high as char);
            out.push(low as char);
        }

        out
    }
}

pub type GlobalID = u64;

//const SpaceSpace: usize = 8; // we only allow 8 unique spaces currently

pub struct Space(usize);

impl Space {
    pub const fn space(i: usize) -> Space {
        assert!(i < 8);

        Space(i)
    }
}

pub fn tid_gen(space: Space) -> GlobalID {
    todo!()
}

impl Instruction {
    pub fn invoke<const N: usize>(inst: &'static str, args: [LLVMArg; N]) -> Instruction {
        //
        Instruction {
            inst: inst.into(),
            args: SmallVec::from_slice(&args),
        }
    }

    pub fn annotype<W: std::io::Write>(&self, w: &mut std::io::BufWriter<W>, indent: usize) {
        let _ = write!(w, "; members: ");

        for _ in 0..indent {
            let _ = write!(w, "  ");
        }

        for a in self.args.clone() {
            let (ty, l) = a.split();
            let _ = write!(w, "{l}:{ty:?}, ");
        }
    }

    pub fn encode<W: std::io::Write>(&self, w: &mut std::io::BufWriter<W>, indent: usize) {
        self.annotype(w, indent);

        for _ in 0..indent {
            let _ = write!(w, "  ");
        }

        match (self.inst.as_str(), self.args.as_slice()) {
            ("load", [into, from]) => {
                /*
                let from_label = self.inputs[0].local_label();
                let into = self.args[1].unwrap();
                let into_label = into.local_label();
                let into_type = into.var_type.encode();

                writeln!(w, "{into_label} = load {into_type}, ptr {from_label}")
                */

                let (from_type, from_label) = from.split();
                let (into_type, into_label) = into.split();

                let _ = writeln!(
                    w,
                    "{into_label} = load {into_type}, {from_type} {from_label}"
                );
            }
            ("store", [from, into]) => {
                /*let from = self.inputs[0];
                let from_label = from.local_label();
                let from_type = from.var_type.encode();
                let into_label = self.inputs[1].local_label();*/

                let (from_type, from_label) = from.split();
                let (into_type, into_label) = into.split();

                //writeln!(w, "{into_label} = load {into_type}, ptr {from_label}");
                //writeln!(w, "; annotype: {}")
                let _ = writeln!(
                    w,
                    "store {from_type} {from_label}, {into_type} {into_label}"
                );
            }
            (
                inst @ ("add" | "sub" | "mul" | "div" | "and" | "or" | "xor" | "shl" | "ashr"
                | "lshr" | "fsub" | "fadd" | "fmul" | "fdiv" | "urem" | "srem" | "frem"),
                [out, op1, op2],
            ) => {
                //let ty = self.inputs[0].var_type.encode();

                //let op1 = self.inputs[0].ident();
                //let op2 = self.inputs[1].ident();
                //let out = self.inputs[2].ident();
                let (ty, out) = out.split();
                let (_, op1) = op1.split();
                let (_, op2) = op2.split();

                let _ = writeln!(w, "{out} = {inst} {ty} {op1}, {op2}");
            }
            ("alloca", [arg]) => {
                let (ty, l) = arg.split();

                let ty = ty
                    .dereferenced()
                    .expect("tried to assign alloca into a non-ptr type");

                let _ = writeln!(w, "{l} = alloca {ty}");
            }
            ("bitcast", [into, from]) => {
                let (from_type, from_label) = from.split();
                let (into_type, into_label) = into.split();

                let _ = writeln!(
                    w,
                    "{into_label} = bitcast {from_type} {from_label} to {into_type}"
                );
            }
            ("phi", [out, start @ ..]) => {
                let (ty, out) = out.split();

                let _ = write!(w, "{out} = phi {ty} ");

                start
                    .array_chunks::<2>()
                    .map(|[value, label]| {
                        //debug_assert!(value.flags.role() == Role::Variable);
                        //debug_assert!(label.flags.role() == Role::Label);
                        debug_assert_matches!(value.content, VarData::VarID(_));
                        debug_assert_matches!(value.content, VarData::Label(_));

                        Some((value, label))
                    })
                    .intersperse(None) // tell it when to put commas ("in between elements")
                    .for_each(|el| match el {
                        Some((v, l)) => {
                            let (_, v) = v.split();
                            let (_, l) = l.split();

                            let _ = write!(w, "[ {v}, {l} ]");
                        }
                        None => {
                            let _ = write!(w, ", ");
                        }
                    });

                let _ = writeln!(w); // iter didn't push a newline
            }
            ("getelementptr", [out, ptr, ar_idx, rest @ ..]) if rest.len() <= 1 => {
                let (ty, result) = out.split();
                let (ptr_ty, ptr) = ptr.split();

                let (ar_idx_ty, ar_idx) = ar_idx.split();

                match rest {
                    // no field, just stride
                    [] => {
                        let _ = writeln!(
                            w,
                            "{result} = getelementptr {ty}, {ptr_ty} {ptr}, {ar_idx_ty} {ar_idx}"
                        );
                    }
                    [field_idx] => {
                        let (field_idx_ty, field_idx) = field_idx.split();
                        let _ = writeln!(w, "{result} = getelementptr {ty}, {ptr_ty} {ptr}, {ar_idx_ty} {ar_idx}, {field_idx_ty} {field_idx}");
                    }
                    _ => unreachable!(),
                }
            }
            (inst @ ("icmp" | "fcmp"), [result, icmp_cond, op1, op2]) => {
                let (_, result) = result.split();
                let (_, cond) = icmp_cond.split();
                let (ty, op1) = op1.split();
                let (_, op2) = op2.split();

                debug_assert_matches!(cond, VarData::ICMPFlag(_));

                let _ = writeln!(w, "{result} = {inst} {cond} {ty} {op1}, {op2}");
            }
            (other, _) => {
                panic!("Instruction {other} failed because of improper args or unrecognized opcode")
            }
        }

        //into.write_all(self.inst.as_bytes()).unwrap();

        /*for arg in self.args {
            into.write_all(arg.encode_usage())
        }*/
    }
}

//#[repr(packed)]
/*pub struct LLVMVar {
    title: BumpStr, // hopefully a u64
    //
    var_type: LLVMType, // a u64
    //is_lval: bool, // represents a modifiable in the stack by reference
    //iid: u32,       // can be turned into a string scoped by outer block
    //context: ,   // represents the outer block
    //iid: IID,
    vardata: VarDataInner,

    flags: LLVMVarFlags,
}*/

#[derive(Copy, Clone)]
#[repr(packed)]
pub struct LLVMArg {
    annotation: BumpStr, // u64, could be moved inside content

    var_type: LLVMType,

    flags: LLVMArgFlags, // u32

    pad: u8, // make sure tag goes directly after so content is on u64

    content: VarData, // align content to u64 boundary
}

#[repr(packed)]
#[derive(Copy)]
pub struct Packed<T>
where
    T: Debug + Copy + Clone,
{
    v: T,
}

impl<T: std::fmt::Debug + Copy + Clone> Clone for Packed<T> {
    fn clone(&self) -> Self {
        Self { v: self.v.copied() }
    }
}

impl<T: std::fmt::Debug + Copy + Clone> std::fmt::Debug for Packed<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.v.copied().fmt(f)
    }
}

#[bitfield]
#[derive(Copy, Clone)]
pub struct LLVMArgFlags {
    //role: Role,
    filler: B16,
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum VarData {
    VarID(Packed<IID>),
    Label(Packed<IID>),
    Immediate(Packed<i32>),
    ICMPFlag(Packed<CMPFlag>),
    Empty(),
}

#[allow(non_snake_case, non_camel_case_types)]
#[derive(Copy, Clone, Debug)]
pub enum CMPFlag {
    i_eq,
    i_ne,
    i_ugt,
    i_uge,
    i_ult,
    i_ule,
    i_sgt,
    i_sge,
    i_slt,
    i_sle,

    f_oeq,
    f_ogt,
    f_oge,
    f_olt,
    f_ole,
    f_one,
    f_ord,

    f_ueq,
    f_ugt,
    f_uge,
    f_ult,
    f_ule,
    f_une,
    f_uno,

    f_false,
    f_true,
}

impl std::ops::FnOnce<()> for CMPFlag {
    type Output = LLVMArg;

    extern "rust-call" fn call_once(self, args: ()) -> Self::Output {
        LLVMArg {
            content: VarData::ICMPFlag(Packed { v: self }),
            ..LLVMArg::basic()
        }
    }
}

impl std::fmt::Display for CMPFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        let s = match self {
            Self::i_eq => "eq",
            Self::i_ne => "ne",
            Self::i_ugt => "ugt",
            Self::i_uge => "uge",
            Self::i_ult => "ult",
            Self::i_ule => "ule",
            Self::i_sgt => "sgt",
            Self::i_sge => "sge",
            Self::i_slt => "slt",
            Self::i_sle => "sle",

            Self::f_oeq => "oeq",
            Self::f_ogt => "ogt",
            Self::f_oge => "oge",
            Self::f_olt => "olt",
            Self::f_ole => "ole",
            Self::f_one => "one",
            Self::f_ord => "ord",

            Self::f_ueq => "ueq",
            Self::f_ugt => "ugt",
            Self::f_uge => "uge",
            Self::f_ult => "ult",
            Self::f_ule => "ule",
            Self::f_une => "une",
            Self::f_uno => "uno",

            Self::f_false => "false",
            Self::f_true => "true",
        };

        write!(f, "{s}")
    }
}

impl std::fmt::Display for VarData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::VarID(Packed { v: iid }) => {
                write!(f, "%{}", iid.ident())
            }
            Self::Label(Packed { v: iid }) => {
                write!(f, "{}", iid.ident())
            }
            Self::Immediate(Packed { v: imm }) => {
                write!(f, "{}", imm)
            }
            Self::ICMPFlag(Packed { v: icmp }) => {
                write!(f, "{}", icmp)
            }
            Self::Empty() => panic!("tried to format a typeonly variable's data"),
        }
    }
}

/*impl VarData {
    pub unsafe fn unpack(role: Role, packed: VarDataInner) -> Self {
        match role {
            Role::Label => VarData::Label(packed.label),
            Role::Variable => VarData::VarID(packed.varid),
            Role::Immediate => VarData::Immediate(packed.immediate),
            Role::ICMPFlag => VarData::ICMPFlag(packed.icmp),
            Role::TypeOnly => VarData::Empty(),
        }
    }
}

#[derive(Copy, Clone)]
pub union VarDataInner {
    varid: IID,
    label: IID,
    immediate: u32,
    icmp: ICMPFlag,
    empty: (),
}*/

assert_eq_size!(LLVMArgFlags, u16);

//assert_eq_size!(IID, U48T);

struct U192T(u64, u64, u64);
assert_eq_size!(LLVMArg, U192T);

#[derive(Copy, Clone, Debug)]
#[repr(packed)]
pub struct IID {
    lid: u32,
    //origin: u16, // the thread that originated the ID
    //block: u16,
}

#[repr(packed)]
pub struct Immediate {}

struct Scope {
    ident: u16,
}

impl IID {
    pub fn new() -> Self {
        todo!()
    }

    pub fn ident(self) -> LLVMIdent {
        todo!()
    }
}

// if it becomes important to align these and
// do non-atomic ops we can approach that later
//#[repr(align)]
struct ThreadLocalState {}

#[derive(BitfieldSpecifier, PartialEq, Eq)]
#[bits = 4]
enum Role {
    Label,
    Variable,
    TypeOnly,
    Immediate,
    ICMPFlag,
}

impl LLVMArgFlags {
    pub fn cleared() -> Self {
        todo!()
    }
}

impl LLVMArg {
    pub fn temp(vtype: LLVMType) -> Self {
        Self {
            var_type: vtype,
            content: VarData::VarID(todo!()),
            //content: VarDataInner { varid: todo!() },
            //iid: IID::new(),
            ..Self::basic()
        }
    }

    pub fn proper(var_type: LLVMType) -> Self {
        Self {
            var_type,
            content: VarData::VarID(todo!()),
            ..Self::basic()
        }
    }

    pub fn label() -> Self {
        Self {
            var_type: LLVMType::undefined(),
            content: VarData::Label(todo!()),
            ..Self::basic()
        }
    }

    pub fn immediate(var_type: LLVMType, val: i32) -> Self {
        Self {
            var_type,
            content: VarData::Immediate(Packed { v: val }),
            ..Self::basic()
        }
    }

    pub fn typeonly(var_type: LLVMType) -> Self {
        Self {
            var_type,
            ..Self::basic()
        }
    }

    pub fn means(self, s: &str) -> Self {
        Self {
            annotation: todo!("intern s"),
            ..self
        }
    }

    fn basic() -> Self {
        Self {
            annotation: BumpStr::empty(),
            var_type: LLVMType::undefined(),
            pad: 0,
            content: VarData::Empty(),
            flags: LLVMArgFlags::new(),
        }
    }

    /// If this type is dereferenceable,
    /// constructs a new var with a decremented
    /// refcount and provides a chunk with a `load`
    pub fn deref(&self) -> Option<LLVMBlob<1>> {
        let new_type = self.var_type.dereferenced()?; // if type not deref, then None as var not
                                                      // deref
        let new_var = LLVMArg::temp(new_type);

        let mut chunk = LLVMChunk::empty();

        chunk.push(Instruction::invoke("load", [new_var, *self]));

        Some((chunk, new_var))
    }

    pub fn bitcast(&self, other: LLVMType) -> LLVMBlob<1> {
        let new_var = Self::temp(other);

        let mut chunk = LLVMChunk::empty();

        chunk.push(Instruction::invoke("bitcast", [new_var, *self]));

        (chunk, new_var)
    }

    /// Take the contents of this var and push them into the stack, returning
    /// a variable referencing them
    pub fn push(&self) -> LLVMBlob<2> {
        let pty = self.var_type.referenced();
        let space = Self::temp(pty);

        let mut chunk = LLVMChunk::empty();

        // make room for us on stack
        chunk.push(Instruction::invoke("alloca", [space]));

        // push us to stack
        chunk.push(Instruction::invoke("store", [*self, space]));

        (chunk, space)
    }

    pub fn split(self) -> (LLVMType, VarData) {
        (self.var_type, self.content)
    }

    pub fn stalloc(var_type: LLVMType) -> LLVMBlob<1> {
        let nv = Self::temp(var_type.referenced());

        let mut chunk = LLVMChunk::empty();

        chunk.push(Instruction::invoke("alloca", [nv]));

        (chunk, nv)
    }

    /// produces an LLVMArg of type i1 with the
    /// requested bit index
    ///
    /// self must be an integral type
    #[rustfmt::skip]
    pub fn get_bit(self, bit_index: usize) -> LLVMBlob<3> {
        let mut chunk = LLVMChunk::empty();

        /*
         * inputs [bit_index, original]
         * outputs [output]
         *
         * shifted := original >> bit_index
         * masked := shifted & 0x1
         * output := masked == 1
         */

        let shifted = LLVMArg::temp(self.var_type);
        let masked = LLVMArg::temp(self.var_type);
        let output = LLVMArg::temp(LLVMPrimitive::i1_t());

        chunk.then("lshr", [shifted, self, LLVMArg::immediate(self.var_type, bit_index as i32)]);
        chunk.then("and", [masked, shifted, LLVMArg::immediate(self.var_type, 0x1)]);
        chunk.then("icmp", [output, CMPFlag::i_eq(), LLVMArg::immediate(self.var_type, 0x1), masked]);

        (chunk, output)

    }

    /// produces an LLVMArg with the bit set to the new desired value
    ///
    /// self must be an integral type
    #[rustfmt::skip]
    pub fn set_bit(self, bit_index: usize, value: bool) -> LLVMBlob<5> {
        let mut chunk = LLVMChunk::empty();

        /*
         * inputs [bit_index, original, value]
         * outputs [output]
         * mask := 0x1 << bit_index
         * negmask := ~mask
         * cleared := original & negmask
         * bit := (value as mask_type) << bit_index
         * output := cleared | bit
         */

        let bit_index = LLVMArg::immediate(self.var_type, bit_index as i32);

        let mask = LLVMArg::temp(self.var_type);

        let negmask = LLVMArg::temp(self.var_type);

        let cleared = LLVMArg::temp(self.var_type);
        let bit = LLVMArg::temp(self.var_type);
        let output = LLVMArg::temp(self.var_type);

        let value = LLVMArg::immediate(self.var_type, if value { 1 } else { 0 });

        chunk.then("shl", [mask, LLVMArg::immediate(self.var_type, 1), bit_index]);
        chunk.then("xor", [negmask, mask, LLVMArg::immediate(self.var_type, -1)]);
        chunk.then("and", [cleared, self, negmask]);
        chunk.then("shl", [bit, value, bit_index]);
        chunk.then("or", [output, cleared, bit]);

        (chunk, output)
    }
}

pub struct LLVMBasicBlock {
    oid: u16, // the id for the block
    chunk: LLVMChunk,
}

/// A "segment" of instructions that do a short action
pub struct LLVMChunk<const INLINE_INSTRUCTIONS: usize = 5> {
    instructions: SmallVec<[Instruction; INLINE_INSTRUCTIONS]>,
}

impl<const INLINE_INSTRUCTIONS: usize> LLVMChunk<INLINE_INSTRUCTIONS> {
    pub fn empty() -> Self {
        Self {
            instructions: SmallVec::new(),
        }
    }

    pub fn push(&mut self, inst: Instruction) {
        self.instructions.push(inst)
    }

    pub fn combine<const A: usize, const B: usize>(
        a: LLVMChunk<A>,
        b: LLVMChunk<B>,
    ) -> LLVMChunk<{ (A + B).min(10) }> {
        todo!()
    }

    pub fn then<const N: usize>(&mut self, inst: &'static str, args: [LLVMArg; N]) {
        self.push(Instruction::invoke(inst, args));
    }

    pub fn blob<const N: usize>(&mut self, other: LLVMBlob<N>) -> LLVMArg {
        let (c, a) = other;

        for inst in c.instructions {
            self.push(inst);
        }

        a
    }
}

const fn limit(a: usize, b: usize) -> usize {
    let c = a + b;
    c.min(10)
}

pub struct LLVMFunctionBlock {
    fid: u64,           // a stringifiable identifier for the function
    entry_block: usize, // the index of the basic block to put "first"
    basic_blocks: Vec<LLVMBasicBlock>,
}

impl LLVMFunctionBlock {
    pub fn encode<W: std::io::Write>(&self, _into: &mut std::io::BufWriter<W>, _indent: usize) {}
}

#[derive(Copy, Clone)]
pub struct BumpStr {
    inner: IStr, // change this at some point
}

impl BumpStr {
    pub fn as_bytes(&self) -> &[u8] {
        // this is fine here since we have static lifetime
        self.inner.resolve().as_bytes()
    }

    /// Returns the empty str as a bumpstr, a special value
    pub fn empty() -> Self {
        todo!()
    }

    pub fn as_str(&self) -> &str {
        todo!()
    }
}

impl Into<BumpStr> for &'static str {
    fn into(self) -> BumpStr {
        todo!()
    }
}

// TODO: see if this can be fully packed into a u64
#[derive(Copy, Clone)]
#[repr(packed)]
pub struct LLVMType {
    reference_depth: u16, // how many pointer layers deep this pointer is to a type
    inner_type: LLVMTargetType,
}

#[repr(u16)]
#[derive(Copy, Clone)]
pub enum LLVMTargetType {
    Primitive(LLVMPrimitive),
    Composite(LLVMComposite),
}

impl std::fmt::Display for LLVMTargetType {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

//assert_eq_size!(LLVMTargetType, u64);
assert_eq_size!(LLVMComposite, u32);
//assert_eq_size!(LLVMPrimitive, u32);

#[derive(Copy, Clone)]
#[repr(packed)]
pub struct LLVMComposite {
    id: LoweredTypeID,
}

#[allow(non_snake_case, non_camel_case_types)]
#[derive(Copy, Clone)]
pub enum LLVMPrimitive {
    void_t,

    i1_t,
    i8_t,
    i16_t,
    i32_t,
    i64_t,
    i128_t,
    i256_t,
    i512_t,
}

assert_eq_size!(LLVMType, u64);
assert_eq_size!(LLVMTargetType, U48T);

#[repr(packed)]
struct U48T(u32, u16);

impl LLVMType {
    pub fn dereferenced(self) -> Option<Self> {
        match self.reference_depth {
            0 => None,
            other => Some(Self {
                reference_depth: other - 1,
                inner_type: self.inner_type,
            }),
        }
    }

    pub fn referenced(self) -> Self {
        Self {
            reference_depth: self.reference_depth + 1,
            inner_type: self.inner_type,
        }
    }

    pub fn void() -> Self {
        Self {
            reference_depth: 0,
            inner_type: LLVMTargetType::Primitive(LLVMPrimitive::void_t),
        }
    }

    pub fn never() -> Self {
        todo!()
    }

    pub fn undefined() -> Self {
        todo!()
    }
}

impl std::fmt::Display for LLVMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.reference_depth {
            0 => {
                todo!()
            }
            _other => {
                write!(f, "ptr")
            }
        }
    }
}

impl std::fmt::Debug for LLVMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "depth {} to inner {}",
            self.reference_depth.copied(), self.inner_type.copied()
        )
    }
}

impl std::ops::FnOnce<()> for LLVMPrimitive {
    type Output = LLVMType;

    extern "rust-call" fn call_once(self, _args: ()) -> Self::Output {
        let t = LLVMType {
            reference_depth: 0,
            inner_type: LLVMTargetType::Primitive(self),
        };
        //LLVMType::Primitive(self)
        //
        t
    }
}
