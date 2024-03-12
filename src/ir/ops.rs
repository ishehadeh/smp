use super::values::VReg;

#[derive(Clone, Debug)]
pub enum IrOp {
    /// IAdd.0 = IAdd.1 + IAdd.2
    IAdd(VReg, VReg, VReg),

    /// ISub.0 = ISub.1 - ISub.2
    ISub(VReg, VReg, VReg),

    /// IDiv.0 = IDiv.1 / IDiv.2
    IDiv(VReg, VReg, VReg),

    /// IMul.0 = IMul.1 * IMul.2
    IMul(VReg, VReg, VReg),

    /// store an immediate integer in a virtual register
    IStoreImm(VReg, i32),

    /// Using params from Call.2 invoke the function in call.1
    /// storing the return value in  Call.0
    Call(VReg, String, Vec<VReg>),

    /// Mark Eq.0 equal to Eq.1 at this point in the program
    Eq(VReg, VReg),
}
