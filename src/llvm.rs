use std::io::Write;

use crate::helper::interner::{IStr, Internable, SpurHelper};
use smallstr::SmallString;
use smallvec::SmallVec;

type LLVMBlob = (LLVMChunk, LLVMVar);

use modular_bitfield::prelude::*;

#[derive(Copy, Clone)]
pub struct LoweredTypeID(u32);

#[derive(Clone)]
pub struct Instruction {
    inst: BumpStr,
    args: SmallVec<[LLVMVar; 3]>,
    //output: Option<LLVMVar>,
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

            out.push(high);
            out.push(low);
        }

        out
    }
}

pub type GlobalID = u64;

//const SpaceSpace: usize = 8; // we only allow 8 unique spaces currently

struct Space(usize);

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
    pub fn invoke<const N: usize>(inst: &'static str, args: [LLVMVar; N]) -> Instruction {
        //
        Instruction {
            inst: BumpStr::from(inst),
            args: SmallVec::from_slice(&args),
        }
    }

    /*pub fn load(input: LLVMVar, output: LLVMVar) -> Instruction {
        //Self::invoke(InstructionType::Load, [input], Some(output))
    }*/

    pub fn annotype<W: std::io::Write>(&self, w: &mut std::io::BufWriter<W>, indent: usize) {
        write!(w, "; members: ");

        for _ in 0..indent {
            write!(w, "  ");
        }

        for a in self.args {
            let (ty, l) = a.split();
            write!(w, "{l}:{ty:?}, ");
        }
    }

    pub fn encode<W: std::io::Write>(&self, w: &mut std::io::BufWriter<W>, indent: usize) {
        self.annotype(w, indent);

        for _ in 0..indent {
            write!(w, "  ");
        }

        match (self.inst.as_str(), self.args.as_slice()) {
            ("load", [from, into]) => {
                /*
                let from_label = self.inputs[0].local_label();
                let into = self.args[1].unwrap();
                let into_label = into.local_label();
                let into_type = into.var_type.encode();

                writeln!(w, "{into_label} = load {into_type}, ptr {from_label}")
                */

                let (from_type, from_label) = from.split();
                let (into_type, into_label) = into.split();

                writeln!(
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
                writeln!(
                    w,
                    "store {from_type} {from_label}, {into_type} {into_label}"
                );
            }
            (
                inst @ ("add" | "sub" | "mul" | "div" | "and" | "or" | "xor" | "ashr" | "lshr"
                | "fsub" | "fadd" | "fmul" | "fdiv" | "urem" | "srem" | "frem"),
                [op1, op2, op3],
            ) => {
                //let ty = self.inputs[0].var_type.encode();

                //let op1 = self.inputs[0].ident();
                //let op2 = self.inputs[1].ident();
                //let out = self.inputs[2].ident();
                let (op1_ty, op1) = op1.split();
                let (_, op2) = op2.split();
                let (_, out) = op3.split();

                writeln!(w, "{out} = {inst} {op1_ty} {op1}, {op2}");
            }
            ("alloca", [arg]) => {
                let (ty, l) = arg.split();

                let ty = ty
                    .dereferenced()
                    .expect("tried to assign alloca into a non-ptr type");

                writeln!(w, "{l} = alloca {ty}");
            }
            ("bitcast", [from, into]) => {
                let (from_type, from_label) = from.split();
                let (into_type, into_label) = into.split();

                writeln!(
                    w,
                    "{into_label} = bitcast {from_type} {from_label} to {into_type}"
                );
            }
            ("phi", [start @ .., end]) => {
                todo!()
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

#[derive(Copy, Clone)]
#[repr(packed)]
pub struct LLVMVar {
    var_type: LLVMType, // a u64
    //is_lval: bool, // represents a modifiable in the stack by reference
    title: BumpStr, // hopefully a u64
    //iid: u32,       // can be turned into a string scoped by outer block
    //context: ,   // represents the outer block
    iid: IID,

    flags: LLVMVarFlags,
}

#[derive(Copy, Clone)]
pub struct IID {
    lid: u32,
    origin: u16, // the thread that originated the ID
                 //block: u16,
}

struct Scope {
    ident: u16,
}

impl IID {
    pub fn new() -> Self {
        //
    }
}

// if it becomes important to align these and
// do non-atomic ops we can approach that later
//#[repr(align)]
struct ThreadLocalState {}

struct U192T(u64, u64, u64);
assert_eq_size!(LLVMVar, U192T);

#[bitfield]
#[derive(Copy, Clone)]
pub struct LLVMVarFlags {
    #[bits = 2]
    role: Role,

    filler: B14,
}

impl LLVMVarFlags {}

assert_eq_size!(LLVMVarFlags, u16);

#[derive(BitfieldSpecifier)]
#[bits = 2]
enum Role {
    Label,
    Variable,
    TypeOnly,
}

impl LLVMVarFlags {
    pub fn cleared() -> Self {
        todo!()
    }
}

impl LLVMVar {
    pub fn temp(vtype: LLVMType) -> Self {
        Self {
            var_type: vtype,
            title: todo!(),
            iid: IID::new(),
            flags: LLVMVarFlags::cleared(),
        }
    }

    pub fn proper(title: BumpStr, var_type: LLVMType) -> Self {
        Self {
            var_type,
            title,
            iid: IID::new(),
            flags: LLVMVarFlags::cleared(),
        }
    }

    pub fn label(title: BumpStr) -> Self {
        Self {
            var_type: LLVMType::void(),
            iid: IID::new(),
            title,
            flags: LLVMVarFlags::new().with_role(Role::Label),
        }
    }

    /// If this type is dereferenceable,
    /// constructs a new var with a decremented
    /// refcount and provides a chunk with a `load`
    pub fn deref(&self) -> Option<LLVMBlob> {
        let new_type = self.var_type.dereferenced()?; // if type not deref, then None as var not
                                                      // deref
        let new_var = LLVMVar::temp(new_type);

        let chunk = LLVMChunk::empty();

        chunk.push(Instruction::invoke("load", [*self, new_var]));

        Some((chunk, new_var))
    }

    pub fn bitcast(&self, other: LLVMType) -> LLVMBlob {
        let new_var = Self::temp(other);

        let chunk = LLVMChunk::empty();

        chunk.push(Instruction::invoke("bitcast", [*self, new_var]));

        (chunk, new_var)
    }

    /// Take the contents of this var and push them into the stack, returning
    /// a variable referencing them
    pub fn push(&self) -> LLVMBlob {
        let pty = self.var_type.referenced();
        let space = Self::temp(pty);

        let chunk = LLVMChunk::empty();

        // make room for us on stack
        chunk.push(Instruction::invoke("alloca", [space]));

        // push us to stack
        chunk.push(Instruction::invoke("store", [*self, space]));

        (chunk, space)
    }

    pub fn split(self) -> (LLVMType, LLVMIdent) {
        (self.var_type, todo!())
    }
}

pub struct LLVMBasicBlock {
    oid: u16, // the id for the block
    chunk: LLVMChunk,
}

/// A "segment" of instructions that do a short action
pub struct LLVMChunk {
    instructions: SmallVec<[Instruction; 5]>,
}

impl LLVMChunk {
    pub fn empty() -> Self {
        Self {
            instructions: SmallVec::new(),
        }
    }

    pub fn push(&mut self, inst: Instruction) {
        self.instructions.push(inst)
    }
}

pub struct LLVMFunctionBlock {
    fid: u64,           // a stringifiable identifier for the function
    entry_block: usize, // the index of the basic block to put "first"
    basic_blocks: Vec<LLVMBasicBlock>,
}

impl LLVMFunctionBlock {
    pub fn encode<W: std::io::Write>(&self, into: &mut std::io::BufWriter<W>, indent: usize) {}
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
}

impl std::fmt::Display for LLVMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.reference_depth {
            0 => {
                todo!()
            }
            other => {
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
            self.reference_depth, self.inner_type
        )
    }
}

impl std::ops::FnOnce<()> for LLVMPrimitive {
    type Output = LLVMType;

    extern "rust-call" fn call_once(self, args: ()) -> Self::Output {
        let t = LLVMType {
            reference_depth: 0,
            inner_type: LLVMTargetType::Primitive(self),
        };
        //LLVMType::Primitive(self)
        //
        t
    }
}
