use std::{collections::HashMap, fmt::format};

use smallstr::SmallString;

use crate::{
    helper::interner::{IStr, Internable},
    llvm::{
        Instruction, LLVMBlob, LLVMChunk, LLVMFunctionBlock, LLVMPrimitive, LLVMType, LLVMVar,
        LoweredTypeID,
    },
};

use std::io::Write;

struct DynamicMemberRepresentation {
    member_type: LoweredTypeID,
}

/// represents an llvm "thing" that is run that
/// outputs an LLVMVar in the current context

#[derive(PartialEq, Eq, Hash)]
struct ConstValue {
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
        todo!()
    }

    pub fn bitfield_check(&self, self_ref: LLVMVar, tag: ConstValue) -> Option<LLVMBlob> {
        let bit_index = *self.dynmem_lookup.get(&tag)?;

        let chunk = LLVMChunk::empty();

        let btype = self.bitfield_type();
        let bval = LLVMVar::temp(btype);

        let bref = LLVMVar::temp(btype.referenced());

        // a bitfield is always the first field of a type that has any bitfield
        chunk.push(Instruction::invoke(
            "getelementptr",
            [
                bref,
                self_ref,
                LLVMVar::immediate(todo!(), LLVMPrimitive::i32_t(), 0),
                LLVMVar::immediate(todo!(), LLVMPrimitive::i32_t(), 0),
            ],
        ));

        chunk.push(Instruction::invoke("load", [bval, bref]));

        let shifted = LLVMVar::temp(btype);

        let shift_amount = LLVMVar::immediate(todo!(), btype, bit_index as u32);

        chunk.push(Instruction::invoke("lshr", [shifted, bval, shift_amount]));

        let mask = LLVMVar::immediate(todo!(), btype, 0x1); // lsb

        let masked = LLVMVar::temp(btype);

        chunk.push(Instruction::invoke("and", [masked, shifted, mask]));

        let has_member = LLVMVar::temp(LLVMPrimitive::i1_t());

        // TODO: comparison
        chunk.push(Instruction::invoke("icmp", [has_member, icmp::eq(), immediate(btype, 1), masked]));

        //chunk.push(Instruction::invoke("getelementptr", args))

        //let out_name = thread::varname();

        Some((chunk, has_member))
    }

    pub fn field(&self, name: IStr) -> Option<LLVMBlob> {
        todo!()
    }

    pub fn dynmem(&self, tag: ConstValue) -> Option<LLVMBlob> {
        todo!()
    }

    /// returns an llvmvar that points into the stack at an unboxed instance of the type
    ///
    /// inits the provided instance variables with the provided llvmvar vals
    pub fn construct(&self, inputs: Vec<(IStr, LLVMVar)>) -> LLVMBlob {
        let (chunk, v) = LLVMVar::stalloc(self.as_llvm());

        todo!()
    }

    pub fn as_llvm(&self) -> LLVMType {
        todo!()
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
    pub fn call(&self, function_ptr: LLVMVar, inputs: Vec<LLVMVar>) -> LLVMChunk {
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
