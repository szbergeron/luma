use std::{collections::HashMap, fmt::format};

use crate::{helper::interner::{IStr, Internable}, llvm::{LLVMPrimitive, LLVMVar}};


struct DynamicMemberRepresentation {
    member_type: LoweredTypeID,
}

/// represents an llvm "thing" that is run that
/// outputs an LLVMVar in the current context

#[derive(PartialEq, Eq, Hash)]
struct ConstValue {
    val: usize,
}

struct LoweredType {
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
    }

    pub fn bitfield_check(&self, self_ref: LLVMVar, tag: ConstValue) -> Option<(LLVMBlock, LLVMVar)> {
        let bit_index = *self.dynmem_lookup.get(&tag)?;

        let out_name = thread::varname();

        let _v = LLVMPrimitive::i1_t();

        let outvar = LLVMVar {
            var_type: LLVMPrimitive::i1_t(),
            is_ref: false,
            title: "".intern(),
            uname: out_name,
        };

        let block = format("")

        None
    }

    pub fn field(&self, name: IStr) -> Option<Reference> {
        todo!()
    }

    pub fn dynmem(&self, tag: ConstValue) -> Option<Reference> {
        todo!()
    }

    /// returns an llvmvar that points into the stack at an unboxed instance of the type
    ///
    /// inits the provided instance variables with the provided llvmvar vals
    pub fn construct(&self, inputs: Vec<(IStr, LLVMVar)>) -> (LLVMBlock, LLVMVar) {
        let stack_alloc_size = self.size();
    }
}

/// Some "fields" or values can be called, or "applied"
///
/// If we can monomorphise here, we do, and we also allow inlining of
/// sufficiently-simple operations at this site
struct Apply {
    sources: Vec<Encoding>,

    parameters: Vec<LoweredTypeID>,
    returns: LoweredTypeID,
}

impl Apply {
    //pub fn set_attributes
    pub fn call(&self, function_ptr: LLVMVar, inputs: Vec<LLVMVar>) -> (LLVMBlock, LLVMVar) {
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

    pub fn encode_inlined(&self) -> (LLVMBlock, LLVMVar) {
        todo!()
    }

    pub fn encode_monomorphized(&self) -> (LLVMBlock, LLVMVar) {
        todo!()
    }

    pub fn encode_virtual(&self) -> (LLVMBlock, LLVMVar) {
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
