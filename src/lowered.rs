use std::{collections::HashMap, fmt::format};

use smallstr::SmallString;

use crate::{
    helper::interner::{IStr, Internable},
    llvm::{
        CMPFlag, Instruction, LLVMArg, LLVMBlob, LLVMChunk, LLVMFunctionBlock, LLVMPrimitive,
        LLVMType, LoweredTypeID,
    },
};

use std::io::Write;

pub struct DynamicMemberRepresentation {
    member_type: LoweredTypeID,
}

/// represents an llvm "thing" that is run that
/// outputs an LLVMVar in the current context

#[derive(PartialEq, Eq, Hash)]
pub struct ConstValue {
    val: usize,
}

/// Note on layout:
///
/// A lowered type starts with a bitfield (if any dynamic members are
/// ever used on the type). The size of the bitfield is controlled
/// by the type, but will be padded out to align the other fields of the type
///
/// Following that bitfield, the type contains all direct fields of the type,
/// in an order defined by the type
///
/// Following the always-fields, the dynamic members each are given an aligned slot
/// in the type. Whether a dynamic member is put in the type by-value
/// or as an allocated out-of-line is determined by a later note,
/// but the given interface allows that decision to be local
/// to the type itself, with the out of line being taken reference to or
/// copied from out-of-line to in-line on access
///
pub struct LoweredType {
    id: LoweredTypeID,
    fields: Vec<(IStr, LoweredTypeID, Option<ConstValue>)>,
    dynmems: Vec<(ConstValue, LoweredTypeID)>,
    dynmem_lookup: HashMap<ConstValue, usize>, // the index in dynmems that a given ConstValue
    // tagged dynmem exists at
    apply_impl: Option<Apply>,
}

impl LoweredType {
    pub fn size(&self) -> usize {
        // all fields are known to be inlined, but need
        // to figure out if the dynamic members should each be inlined too
        //let regular_fields_size =
        todo!("size may be unknowable before packing done by llvm")
    }

    pub fn encode_type_reference(&self) -> SmallString<[u8; 16]> {
        todo!()
    }

    pub fn encode_type_definition<W: std::io::Write>(&self, w: &mut std::io::BufWriter<W>) {
        let typename = self.encode_type_reference();

        write!(w, "{typename} = type {{ {{}}, ");

        self.fields
            .iter()
            .map(|(name, ty, init)| {
                println!("field");
                Some((*ty, false))
            })
            .chain(self.dynmems.iter().map(|(cv, ty)| {
                println!("dynmem");
                Some((*ty, true))
            }))
            .intersperse(None)
            .for_each(|v| {
                // comma or the field
                match v {
                    None => {
                        write!(w, ", ");
                    }
                    Some((ty, is_dynmem)) => {
                        // TODO: dynmems require additional bitfield stuff and potential OOLing
                        write!(w, "{}", ty.resolve(todo!()).encode_type_reference());
                    }
                };
            });

        write!(w, "}}");
    }

    // in "powers of 2"
    pub fn bitfield_type(&self) -> LLVMType {
        let count = self.dynmems.len();
        match count {
            0..8 => LLVMPrimitive::i8_t(),
            8..16 => LLVMPrimitive::i16_t(),
            16..32 => LLVMPrimitive::i32_t(),
            32..64 => LLVMPrimitive::i64_t(),
            64..128 => LLVMPrimitive::i128_t(),
            128..256 => LLVMPrimitive::i256_t(),
            _ => panic!("currently only 255 distinct dynmems are allowed on any given type"),
        }
    }

    pub fn bitfield_check(&self, self_ref: LLVMArg, tag: ConstValue) -> Option<LLVMBlob<1>> {
        let bit_index = *self.dynmem_lookup.get(&tag)?;

        let mut chunk = LLVMChunk::empty();

        let btype = self.bitfield_type();

        let bref = LLVMArg::temp(btype.referenced());

        // a bitfield is always the first field of a type that has any bitfield
        let zero = LLVMArg::immediate(LLVMPrimitive::i32_t(), 0);
        chunk.then("getelementptr", [bref, self_ref, zero, zero]);

        let derefd: LLVMArg = chunk.blob(bref.deref().unwrap());

        let bval = chunk.blob(derefd.get_bit(bit_index));

        Some((chunk, bval))

        //chunk.push(Instruction::invoke("getelementptr", args))

        //let out_name = thread::varname();

        //Some((chunk, has_member))
    }

    pub fn bitfield_set(&self, self_ref: LLVMArg, tag: ConstValue, val: bool) -> Option<LLVMChunk<10>> {
        let bit_index = *self.dynmem_lookup.get(&tag)?;

        let mut chunk = LLVMChunk::empty();

        let btype = self.bitfield_type();

        let bref = LLVMArg::temp(btype.referenced());

        let zero = LLVMArg::immediate(LLVMPrimitive::i32_t(), 0);
        chunk.then("getelementptr", [bref, self_ref, zero, zero]);

        let derefd = chunk.blob(bref.deref().unwrap());

        let bval = chunk.blob(derefd.set_bit(bit_index, val));

        chunk.then("store", [bref, bval]);

        Some(chunk)
    }

    pub fn field(&self, self_ref: LLVMArg, name: IStr) -> Option<(LLVMChunk, LVal)> {
        let chunk = LLVMChunk::<5>::empty();

        todo!()
    }

    pub fn dynmem(&self, self_ref: LLVMArg, tag: ConstValue) -> Option<DynMem> {
        let chunk = LLVMChunk::<5>::empty();

        todo!()
    }

    /// returns an llvmvar that points into the stack at an unboxed instance of the type
    ///
    /// inits the provided instance variables with the provided llvmvar vals
    pub fn construct(&self, inputs: Vec<(IStr, LLVMArg)>) -> LLVMBlob<1> {
        let (mut chunk, v) = LLVMArg::stalloc(self.as_llvm());

        for (fname, fval) in inputs {
            let (mut f, lv) = self.field(v, fname).unwrap();
            let rv = f.blob(lv.to_rval());

            let fref = chunk.blob((f, rv));

            chunk.then("store", [fval, fref]);
            //chunk.blob(fref.unwrap())
        }

        (chunk, v)
    }

    pub fn as_llvm(&self) -> LLVMType {
        todo!()
    }
}

#[derive(Clone, Copy)]
pub struct LVal(LLVMArg);

pub struct DynMem {
    base_type: LoweredTypeID,
    tag: ConstValue,
}

impl DynMem {
    /// convert this DynMem into an RVal by unwrapping
    /// the tagged reference, panic on nonexistence
    pub fn unwrap(&self) -> (LLVMChunk, LLVMChunk) {
        let panic_edge = LLVMArg::label();
        let contains_edge = LLVMArg::label();

        let (mut chunk, _has_member, _member) = self.checked();

        chunk.push(todo!());

        todo!()
    }

    pub fn assign(&self, val: LLVMArg) -> LLVMBlob<5> {
        todo!()
    }

    /// First var is an i1 of whether the second var contains
    /// a populated RVal to the target of the DynMem
    pub fn checked(&self) -> (LLVMChunk, LLVMArg, LLVMArg) {
        todo!()
    }
}

impl LVal {
    pub fn to_rval(self) -> LLVMBlob<1> {
        let inner = self.0;

        inner.deref().unwrap()
    }
}

/// Some "fields" or values can be called, or "applied"
///
/// If we can monomorphise here, we do, and we also allow inlining of
/// sufficiently-simple operations at this site
struct Apply {
    sources: Vec<LLVMFunctionBlock>,

    parameters: Vec<LoweredTypeID>,
    returns: LoweredTypeID,
}

impl Apply {
    //pub fn set_attributes
    pub fn call(&self, function_ptr: LLVMArg, inputs: Vec<LLVMArg>) -> LLVMChunk {
        match (self.should_inline(), self.should_monomorphize()) {
            (true, true) => {
                // the rare case that we can actually just stick it in
                //self.encode_inlined()
                //
                // for now, we don't do recursion detection so we just do
                // monomorphized encode and rely on LLVM inlining for simple ops
                self.encode_monomorphized()
            }
            (false, true) => {
                // we know what we're actually calling, so we can emit the label ref
                self.encode_monomorphized()
            }
            (_, false) => {
                // we can't monomorphise, so we don't care whether we
                // could be inlined as we have to do a virtual call anyway
                self.encode_virtual()
            }
        }
    }

    pub fn encode_inlined(&self) -> LLVMChunk {
        todo!()
    }

    pub fn encode_monomorphized(&self) -> LLVMChunk {
        todo!()
    }

    pub fn encode_virtual(&self) -> LLVMChunk {
        todo!()
    }

    pub fn should_inline(&self) -> bool {
        todo!()
    }

    pub fn should_monomorphize(&self) -> bool {
        todo!()
    }

    pub fn return_type(&self) -> LoweredTypeID {
        todo!()
    }
}
