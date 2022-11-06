use std::io::Write;

use crate::helper::interner::{IStr, Internable, SpurHelper};
use smallstr::SmallString;
use smallvec::SmallVec;

type LLVMBlob = (LLVMChunk, LLVMVar);

#[derive(Copy, Clone)]
struct LoweredTypeID(u32);

#[derive(Clone)]
struct Instruction {
    inst: BumpStr,
    args: SmallVec<[LLVMVar; 3]>,
    //output: Option<LLVMVar>,
}

struct LLVMValue {}

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

impl Instruction {
    pub fn invoke<const N: usize>(
        inst: &'static str,
        args: [LLVMVar; N],
        rval: Option<LLVMVar>,
    ) -> Instruction {
        //
        todo!()
    }

    /*pub fn load(input: LLVMVar, output: LLVMVar) -> Instruction {
        //Self::invoke(InstructionType::Load, [input], Some(output))
    }*/

    pub fn encode<W: std::io::Write>(&self, w: &mut std::io::BufWriter<W>, indent: usize) {
        for _ in 0..indent {
            w.write_all(&[b'\n']).unwrap();
        }

        match self.inst.resolve() {
            "load" => {
                let from_label = self.inputs[0].local_label();
                let into = self.args[1].unwrap();
                let into_label = into.local_label();
                let into_type = into.var_type.encode();

                writeln!(w, "{into_label} = load {into_type}, ptr {from_label}")
            }
            "store" => {
                let from = self.inputs[0];
                let from_label = from.local_label();
                let from_type = from.var_type.encode();
                let into_label = self.inputs[1].local_label();

                writeln!(w, "store {from_type} {from_label}, ptr {into_label}")
            }
            inst @ ("add" | "sub" | "mul" | "div" | "and" | "or" | "xor" | "ashr" | "lshr"
            | "fsub" | "fadd" | "fmul" | "fdiv" | "urem" | "srem" | "frem") => {
                let ty = self.inputs[0].var_type.encode();

                let op1 = self.inputs[0].local_label();
                let op2 = self.inputs[1].local_label();
                let out = self.inputs[2].local_label();

                writeln!(w, "{out} = {inst} {ty} {op1}, {op2}");
            }
            "alloca" => {
            }
        }

        //into.write_all(self.inst.as_bytes()).unwrap();

        /*for arg in self.args {
            into.write_all(arg.encode_usage())
        }*/
    }
}

#[derive(Copy, Clone)]
pub struct LLVMVar {
    var_type: LLVMType, // a u64
    //is_lval: bool, // represents a modifiable in the stack by reference
    title: BumpStr, // hopefully a u64
    iid: u32,       // can be turned into a string scoped by outer block
}

impl LLVMVar {
    /// encodes the var in rval context, with a
    pub fn encode_read<W: std::io::Write>(&self, into: &mut std::io::BufWriter<W>) {}

    pub fn encode_() {}

    /// If this type is dereferenceable,
    /// constructs a new var with a decremented
    /// refcount and provides a chunk with a `load`
    pub fn deref(&self) -> Option<LLVMBlob> {
        let new_type = self.var_type.deref()?;
        let new_var = Self {
            iid: todo!(),
            var_type: new_type,
            title: self.title,
        };

        let chunk = LLVMChunk::empty();

        chunk.push(Instruction::invoke("load", []))
    }

    /// Take the contents of this var and push them into the stack, returning
    /// a variable referencing them
    pub fn push(&self) -> LLVMBlob {
        //
    }
}

struct LLVMBasicBlock {
    oid: u16, // the id for the block
    chunk: LLVMChunk,
}

/// A "segment" of instructions that do a short action
struct LLVMChunk {
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

struct LLVMFunctionBlock {
    fid: u64,           // a stringifiable identifier for the function
    entry_block: usize, // the index of the basic block to put "first"
    basic_blocks: Vec<LLVMBasicBlock>,
}

impl LLVMFunctionBlock {
    pub fn encode<W: std::io::Write>(&self, into: &mut std::io::BufWriter<W>, indent: usize) {}
}

#[derive(Copy, Clone)]
struct BumpStr {
    inner: IStr, // change this at some point
}

impl BumpStr {
    pub fn as_bytes(&self) -> &[u8] {
        // this is fine here since we have static lifetime
        self.inner.resolve().as_bytes()
    }
}

// TODO: see if this can be fully packed into a u64
#[repr(packed)]
pub struct LLVMType {
    reference_depth: u16, // how many pointer layers deep this pointer is to a type
    inner_type: LLVMTargetType,
}

assert_eq_size!(LLVMType, u64);

impl LLVMType {
    pub fn deref(self) -> Option<Self> {
        match self.reference_depth {
            0 => None,
            other => Some(Self {
                reference_depth: other - 1,
                inner_type: self.inner_type,
            }),
        }
    }
}

#[repr(u16)]
pub enum LLVMTargetType {
    Primitive(LLVMPrimitive),
    Composite(LLVMComposite),
}

pub struct LLVMComposite {
    id: LoweredTypeID,
}

#[repr(u32)]
#[allow(non_snake_case, non_camel_case_types)]
pub enum LLVMPrimitive {
    void_t,

    i1_t,
    i8_t,
    i16_t,
    i32_t,
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
