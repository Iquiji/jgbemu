/// Register or Indirect Memory access
#[derive(Debug, Clone)]
pub enum Location8Bit {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    /// Address pointed to by HL, (HL)
    Indirect,
}

// 16 Bit Registers
#[derive(Debug, Clone)]
pub enum Location16Bit {
    AF,
    BC,
    DE,
    HL,
    SP,
}

#[derive(Debug, Clone)]
pub enum InstructionType {
    Mem(MemoryInstruction),
    Arithmetic(ArithmeticInstruction),
    ShiftRotate(ShiftRotateInstruction),
    SingleBit(SingleBitInstruction),
    Control(ControlInstruction),
    Jump(JumpInstruction),
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub itype: InstructionType,
    pub cycles: u8,
}

#[derive(Debug, Clone)]
pub enum ALUOpTypes{
    ADD,
    ADC,
    SUB,
    SBC,
    AND,
    XOR,
    OR,
    CP,
}

#[derive(Debug, Clone)]
pub enum ArithmeticInstruction{
    ALUReg8(ALUOpTypes, Location8Bit),
    ALUImediate(ALUOpTypes, u8),
    INC8(Location8Bit),
    DEC8(Location8Bit),
    /// add HL,rr
    ADDHL16(Location16Bit),
    INC16(Location16Bit),
    DEC16(Location16Bit),
    ADDSPRelative(i8),
    LoadSPIntoHLRelative(i8),
    /// decimal adjust A
    DAA,
    /// CPL: A = A xor FF, so invert A bitwise
    CPL,
}
/// DAA?
/// RLCA, RRCA, RLA, RRA?
#[derive(Debug, Clone)]
pub enum ShiftRotateType{
    RLC,
    RRC,
    RL,
    RR,
    SLA,
    SRA,
    SWAP,
    SRL,
}

#[derive(Debug, Clone)]
pub enum ShiftRotateInstruction{
    ShiftRotate(ShiftRotateType, Location8Bit),
}

#[derive(Debug, Clone)]
pub enum SingleBitInstruction{
    /// u8 means which Bit
    TestBit(u8, Location8Bit),
    ResetBit(u8, Location8Bit),
    SetBit(u8, Location8Bit),
}

#[derive(Debug, Clone)]
pub enum ControlInstruction{
    /// cy = !cy
    CCF,
    /// cy = 1
    SCF,
    NOP,
    /// Halt till interrupt occurs
    HALT,
    STOP,
    /// Disable Interrupts
    DI,
    /// Enable Interrupts
    EI,
}

#[derive(Debug, Clone)]
pub enum JumpCondition{
    NZ,
    Z,
    NC,
    C,
}

#[derive(Debug, Clone)]
pub enum JumpInstruction{
    JumpConstant(u16),
    JumpHL,
    JumpConstantConditional(JumpCondition, u16),
    JumpRelative(i8),
    JumpRelativeConditional(JumpCondition, i8),
    CallConstant(u16),
    CallConstantConditional(JumpCondition, u16),
    Return,
    ReturnConditional(JumpCondition),
    ReturnEnableInterrupts,
    /// call to 00,08,10,18,20,28,30,38
    Reset(u8),
}

/// Includes Load and Store and general Memory moving instructions
#[derive(Debug, Clone)]
pub enum MemoryInstruction {
    LoadImmediate8(Location8Bit, u8),
    LoadImmediate16(Location16Bit, u16),
    /// See LoadInstruction::LoadIndirectFromA
    LoadIndirectToA(Location16Bit, bool, bool),
    /// LD (BC), A --- LD (HL+), A
    /// Location, Increment?, Decrement?
    LoadIndirectFromA(Location16Bit, bool, bool),
    /// Dest, Src
    Load(Location8Bit, Location8Bit),
    /// read from io-port n (memory FF00+n)
    LoadPortN(u8),
    /// write to io-port n (memory FF00+n)
    StorePortN(u8),
    LoadPortC,
    StorePortC,
    Push(Location16Bit),
    Pop(Location16Bit),
}

#[derive(Debug, Clone)]
pub enum ParseInstructionResult {
    NeedMoreBytes,
    Instruction(Instruction),
}

impl Instruction {
    /// Returns Instruction or needs more Bytes :)
    pub fn parse_from_bytes(bytes: &[u8]) -> ParseInstructionResult {
        // Prefixes:
        if [0xCB, 0xDD, 0xED, 0xFD].contains(bytes.last().unwrap()) {
            return ParseInstructionResult::NeedMoreBytes;
        }

        /*
        Opcode Decoding:
        See https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
        Upon establishing the opcode, the Z80's path of action is generally dictated by these values:

        x = the opcode's 1st octal digit (i.e. bits 7-6)
        y = the opcode's 2nd octal digit (i.e. bits 5-3)
        z = the opcode's 3rd octal digit (i.e. bits 2-0)
        p = y rightshifted one position (i.e. bits 5-4)
        q = y modulo 2 (i.e. bit 3)
         */

        if [0xCB, 0xDD, 0xED, 0xFD].contains(&bytes[0]) {
            // Prefixed Instruction: Ignore
            eprintln!("Unkown Prefixed Instruction");
            return ParseInstructionResult::Instruction(Instruction {
                itype: InstructionType::Unknown,
                cycles: 0,
            });
        }

        let opcode_byte = bytes.last().unwrap();
        let x: u8 = (opcode_byte & 0b1100_0000) >> 6;
        let y: u8 = (opcode_byte & 0b0011_1000) >> 3;
        let z: u8 = opcode_byte & 0b0000_0111;
        let p: u8 = y >> 1;
        let q: u8 = y % 2;
        println!("x: {x}, y: {y}, z: {z}, p: {p}, q: {q}");



        eprintln!("Unkown Unprefixed Instruction");
        ParseInstructionResult::Instruction(Instruction {
            itype: InstructionType::Unknown,
            cycles: 0,
        })
    }
}
