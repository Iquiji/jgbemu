/// Register or Indirect Memory access
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location16Bit {
    AF,
    BC,
    DE,
    HL,
    SP,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstructionType {
    Mem(MemoryInstruction),
    Arithmetic(ArithmeticInstruction),
    ShiftRotate(ShiftRotateInstruction),
    SingleBit(SingleBitInstruction),
    Control(ControlInstruction),
    Jump(JumpInstruction),
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    /// Magic numbers regarding cycle count:
    /// 64 ^= Conditional so we don't know
    /// 255 ^= indefinete
    /// 128 ^= until interupt
    pub cycles: u8,
    pub itype: InstructionType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ALUOpTypes {
    ADD,
    ADC,
    SUB,
    SBC,
    AND,
    XOR,
    OR,
    CP,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArithmeticInstruction {
    /// alu[y] r[z]
    /// Operate on accumulator and register/memory location
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftRotateType {
    RLC,
    RRC,
    RL,
    RR,
    SLA,
    SRA,
    SWAP,
    SRL,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShiftRotateInstruction {
    ShiftRotate(ShiftRotateType, Location8Bit),
    /// rotate A left
    RLCA,
    /// rotate A right
    RRCA,
    /// rotate A left through carry
    RLA,
    /// rotate A right through carry
    RRA,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SingleBitInstruction {
    /// u8 means which Bit
    TestBit(u8, Location8Bit),
    ResetBit(u8, Location8Bit),
    SetBit(u8, Location8Bit),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ControlInstruction {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpCondition {
    NZ,
    Z,
    NC,
    C,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JumpInstruction {
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// SP=HL
    LoadSPFromHL,
    /// LD (nn), SP
    LoadSPIntoConstantAddress(u16),
    /// LD HL, SP+ d
    LoadHLFromSPOffset(i8),
    /// LD A, (nn)
    LoadIndirectConstantToA(u16),
    /// LD (nn), A
    LoadIndirectConstantFromA(u16),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PRes {
    NeedMoreBytes,
    /// How many MORE bytes do we need :)
    NeedMoreBytesSpecific(u8),
    Instruction(Instruction),
}

impl Instruction {
    /// Returns Instruction or needs more Bytes :)
    pub fn parse_from_bytes(bytes: &[u8]) -> PRes {
        const UNKNOWN_INST: Instruction = Instruction {
            itype: InstructionType::Unknown,
            cycles: 0,
        };
        // SEE: https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
        const R_TABLE: [Location8Bit; 8] = [
            Location8Bit::B,
            Location8Bit::C,
            Location8Bit::D,
            Location8Bit::E,
            Location8Bit::H,
            Location8Bit::L,
            Location8Bit::Indirect,
            Location8Bit::A,
        ];
        const RP_TABLE: [Location16Bit; 4] = [
            Location16Bit::BC,
            Location16Bit::DE,
            Location16Bit::HL,
            Location16Bit::SP,
        ];
        const RP2_TABLE: [Location16Bit; 4] = [
            Location16Bit::BC,
            Location16Bit::DE,
            Location16Bit::HL,
            Location16Bit::AF,
        ];
        const CC_TABLE: [JumpCondition; 4] = [
            JumpCondition::NZ,
            JumpCondition::Z,
            JumpCondition::NC,
            JumpCondition::C,
        ];
        const ALU_TABLE: [ALUOpTypes; 8] = [
            ALUOpTypes::ADD,
            ALUOpTypes::ADC,
            ALUOpTypes::SUB,
            ALUOpTypes::SBC,
            ALUOpTypes::AND,
            ALUOpTypes::XOR,
            ALUOpTypes::OR,
            ALUOpTypes::CP,
        ];
        const ROT_TABLE: [ShiftRotateType; 8] = [
            ShiftRotateType::RLC,
            ShiftRotateType::RRC,
            ShiftRotateType::RL,
            ShiftRotateType::RR,
            ShiftRotateType::SLA,
            ShiftRotateType::SRA,
            ShiftRotateType::SWAP,
            ShiftRotateType::SRL,
        ];

        match bytes {
            [0xDD] | [0xFD] => PRes::NeedMoreBytes,
            [0xCB] => PRes::NeedMoreBytes,
            [0xCB, _] => {
                let opcode_byte = bytes[1];
                let x: u8 = (opcode_byte & 0b1100_0000) >> 6;
                let y: u8 = (opcode_byte & 0b0011_1000) >> 3;
                let z: u8 = opcode_byte & 0b0000_0111;

                match x {
                    0 => PRes::Instruction(Instruction {
                        cycles: if z != 6 { 8 } else { 12 },
                        itype: InstructionType::ShiftRotate(ShiftRotateInstruction::ShiftRotate(
                            ROT_TABLE[y as usize],
                            R_TABLE[z as usize],
                        )),
                    }),
                    1 => PRes::Instruction(Instruction {
                        cycles: if z != 6 { 8 } else { 12 },
                        itype: InstructionType::SingleBit(SingleBitInstruction::TestBit(
                            y,
                            R_TABLE[z as usize],
                        )),
                    }),
                    2 => PRes::Instruction(Instruction {
                        cycles: if z != 6 { 8 } else { 16 },
                        itype: InstructionType::SingleBit(SingleBitInstruction::ResetBit(
                            y,
                            R_TABLE[z as usize],
                        )),
                    }),
                    3 => PRes::Instruction(Instruction {
                        cycles: if z != 6 { 8 } else { 16 },
                        itype: InstructionType::SingleBit(SingleBitInstruction::SetBit(
                            y,
                            R_TABLE[z as usize],
                        )),
                    }),
                    _ => unreachable!(),
                }
            }
            [_] => {
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

                let opcode_byte = bytes[0];
                let x: u8 = (opcode_byte & 0b1100_0000) >> 6;
                let y: u8 = (opcode_byte & 0b0011_1000) >> 3;
                let z: u8 = opcode_byte & 0b0000_0111;
                let p: u8 = y >> 1;
                let q: u8 = y % 2;
                // println!("decode opcode single: x: {x}, y: {y}, z: {z}, p: {p}, q: {q}");

                match (x, z, y) {
                    (0, 0, 0) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::Control(ControlInstruction::NOP),
                    }),
                    (0, 0, 1) => PRes::NeedMoreBytesSpecific(2),
                    (0, 0, 2) => PRes::Instruction(Instruction {
                        cycles: 255,
                        itype: InstructionType::Control(ControlInstruction::STOP),
                    }),
                    (0, 0, 3) => PRes::NeedMoreBytesSpecific(1),
                    (0, 0, _) => PRes::NeedMoreBytesSpecific(1),
                    (0, 1, _) => {
                        if q == 0 {
                            PRes::NeedMoreBytesSpecific(2)
                        } else {
                            PRes::Instruction(Instruction {
                                cycles: 8,
                                itype: InstructionType::Arithmetic(ArithmeticInstruction::ADDHL16(
                                    RP_TABLE[p as usize],
                                )),
                            })
                        }
                    }
                    (0, 2, _) => {
                        const X0Z2TABLE: [Instruction; 8] = [
                            Instruction {
                                cycles: 8,
                                itype: InstructionType::Mem(MemoryInstruction::LoadIndirectFromA(
                                    Location16Bit::BC,
                                    false,
                                    false,
                                )),
                            },
                            Instruction {
                                cycles: 8,
                                itype: InstructionType::Mem(MemoryInstruction::LoadIndirectFromA(
                                    Location16Bit::DE,
                                    false,
                                    false,
                                )),
                            },
                            Instruction {
                                cycles: 8,
                                itype: InstructionType::Mem(MemoryInstruction::LoadIndirectFromA(
                                    Location16Bit::HL,
                                    true,
                                    false,
                                )),
                            },
                            Instruction {
                                cycles: 8,
                                itype: InstructionType::Mem(MemoryInstruction::LoadIndirectFromA(
                                    Location16Bit::HL,
                                    false,
                                    true,
                                )),
                            },
                            Instruction {
                                cycles: 8,
                                itype: InstructionType::Mem(MemoryInstruction::LoadIndirectToA(
                                    Location16Bit::BC,
                                    false,
                                    false,
                                )),
                            },
                            Instruction {
                                cycles: 8,
                                itype: InstructionType::Mem(MemoryInstruction::LoadIndirectToA(
                                    Location16Bit::DE,
                                    false,
                                    false,
                                )),
                            },
                            Instruction {
                                cycles: 8,
                                itype: InstructionType::Mem(MemoryInstruction::LoadIndirectToA(
                                    Location16Bit::HL,
                                    true,
                                    false,
                                )),
                            },
                            Instruction {
                                cycles: 8,
                                itype: InstructionType::Mem(MemoryInstruction::LoadIndirectToA(
                                    Location16Bit::HL,
                                    false,
                                    true,
                                )),
                            },
                        ];

                        let idx = q << 2 | p;

                        PRes::Instruction(X0Z2TABLE[idx as usize].clone())
                    }
                    (0, 3, _) => {
                        if q == 0 {
                            PRes::Instruction(Instruction {
                                cycles: 8,
                                itype: InstructionType::Arithmetic(ArithmeticInstruction::INC16(
                                    RP_TABLE[p as usize],
                                )),
                            })
                        } else {
                            PRes::Instruction(Instruction {
                                cycles: 8,
                                itype: InstructionType::Arithmetic(ArithmeticInstruction::DEC16(
                                    RP_TABLE[p as usize],
                                )),
                            })
                        }
                    }
                    (0, 4, _) => PRes::Instruction(Instruction {
                        cycles: if y != 6 { 4 } else { 12 },
                        itype: InstructionType::Arithmetic(ArithmeticInstruction::INC8(
                            R_TABLE[y as usize],
                        )),
                    }),
                    (0, 5, _) => PRes::Instruction(Instruction {
                        cycles: if y != 6 { 4 } else { 12 },
                        itype: InstructionType::Arithmetic(ArithmeticInstruction::DEC8(
                            R_TABLE[y as usize],
                        )),
                    }),
                    (0, 6, _) => PRes::NeedMoreBytesSpecific(1),
                    (0, 7, 0) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::ShiftRotate(ShiftRotateInstruction::RLCA),
                    }),
                    (0, 7, 1) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::ShiftRotate(ShiftRotateInstruction::RRCA),
                    }),
                    (0, 7, 2) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::ShiftRotate(ShiftRotateInstruction::RLA),
                    }),
                    (0, 7, 3) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::ShiftRotate(ShiftRotateInstruction::RRA),
                    }),
                    (0, 7, 4) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::Arithmetic(ArithmeticInstruction::DAA),
                    }),
                    (0, 7, 5) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::Arithmetic(ArithmeticInstruction::CPL),
                    }),
                    (0, 7, 6) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::Control(ControlInstruction::SCF),
                    }),
                    (0, 7, 7) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::Control(ControlInstruction::CCF),
                    }),
                    // +++ x = 1 +++
                    (1, 6, 6) => {
                        PRes::Instruction(Instruction {
                            cycles: 128, // MAGIC CONSTANT?
                            itype: InstructionType::Control(ControlInstruction::HALT),
                        })
                    }
                    (1, _, _) => PRes::Instruction(Instruction {
                        cycles: if y != 6 && z != 6 { 4 } else { 8 },
                        itype: InstructionType::Mem(MemoryInstruction::Load(
                            R_TABLE[y as usize],
                            R_TABLE[z as usize],
                        )),
                    }),
                    // +++ x = 2 +++
                    (2, _, _) => PRes::Instruction(Instruction {
                        cycles: if z != 6 { 4 } else { 8 },
                        itype: InstructionType::Arithmetic(ArithmeticInstruction::ALUReg8(
                            ALU_TABLE[y as usize],
                            R_TABLE[z as usize],
                        )),
                    }),
                    // +++ x = 3 +++
                    (3, 0, 0 | 1 | 2 | 3) => {
                        PRes::Instruction(Instruction {
                            cycles: 64, // MAGIC NUMBER
                            itype: InstructionType::Jump(JumpInstruction::ReturnConditional(
                                CC_TABLE[y as usize],
                            )),
                        })
                    }
                    (3, 0, 4) => PRes::NeedMoreBytesSpecific(1),
                    (3, 0, 5) => PRes::NeedMoreBytesSpecific(1),
                    (3, 0, 6) => PRes::NeedMoreBytesSpecific(1),
                    (3, 0, 7) => PRes::NeedMoreBytesSpecific(1),
                    (3, 1, _) => {
                        if q == 0 {
                            PRes::Instruction(Instruction {
                                cycles: 12,
                                itype: InstructionType::Mem(MemoryInstruction::Pop(
                                    RP2_TABLE[p as usize],
                                )),
                            })
                        } else {
                            let table: [Instruction; 4] = [
                                Instruction {
                                    cycles: 16,
                                    itype: InstructionType::Jump(JumpInstruction::Return),
                                },
                                Instruction {
                                    cycles: 16,
                                    itype: InstructionType::Jump(
                                        JumpInstruction::ReturnEnableInterrupts,
                                    ),
                                },
                                Instruction {
                                    cycles: 4,
                                    itype: InstructionType::Jump(JumpInstruction::JumpHL),
                                },
                                Instruction {
                                    cycles: 8,
                                    itype: InstructionType::Mem(MemoryInstruction::LoadSPFromHL),
                                },
                            ];
                            PRes::Instruction(table[p as usize].clone())
                        }
                    }
                    (3, 2, 0 | 1 | 2 | 3 | 5 | 7) => PRes::NeedMoreBytesSpecific(2),
                    (3, 2, 4) => PRes::Instruction(Instruction {
                        cycles: 8,
                        itype: InstructionType::Mem(MemoryInstruction::StorePortC),
                    }),
                    (3, 2, 6) => PRes::Instruction(Instruction {
                        cycles: 8,
                        itype: InstructionType::Mem(MemoryInstruction::LoadPortC),
                    }),
                    (3, 3, 0) => PRes::NeedMoreBytesSpecific(2),
                    (3, 3, 6) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::Control(ControlInstruction::DI),
                    }),
                    (3, 3, 7) => PRes::Instruction(Instruction {
                        cycles: 4,
                        itype: InstructionType::Control(ControlInstruction::EI),
                    }),
                    (3, 3, _) => {
                        panic!("INVALID INSTRUCTION!!!!");
                    }
                    (3, 4, 0 | 1 | 2 | 3) => PRes::NeedMoreBytesSpecific(2),
                    (3, 4, _) => {
                        panic!("INVALID INSTRUCTION!!!!");
                    }
                    (3, 5, _) => {
                        if q == 0 {
                            PRes::Instruction(Instruction {
                                cycles: 16,
                                itype: InstructionType::Mem(MemoryInstruction::Push(
                                    RP2_TABLE[p as usize],
                                )),
                            })
                        } else if p == 0 {
                            PRes::NeedMoreBytesSpecific(2)
                        } else {
                            panic!("INVALID INSTRUCTION!!!!");
                        }
                    }
                    (3, 6, _) => PRes::NeedMoreBytesSpecific(1),
                    (3, 7, _) => PRes::Instruction(Instruction {
                        cycles: 16,
                        itype: InstructionType::Jump(JumpInstruction::Reset(y * 8)),
                    }),

                    (_, _, _) => {
                        eprintln!("Unkown Unprefixed Instruction, first check: {}", line!());
                        PRes::Instruction(UNKNOWN_INST.clone())
                    }
                }
            }
            [_, ..] => {
                let opcode_byte = bytes[0];
                let x: u8 = (opcode_byte & 0b1100_0000) >> 6;
                let y: u8 = (opcode_byte & 0b0011_1000) >> 3;
                let z: u8 = opcode_byte & 0b0000_0111;
                let p: u8 = y >> 1;
                let q: u8 = y % 2;
                // println!("decode opcode single: x: {x}, y: {y}, z: {z}, p: {p}, q: {q}");

                fn parse_nn(bytes: &[u8]) -> u16 {
                    u16::from_le_bytes([bytes[1], bytes[2]])
                }
                fn parse_n(bytes: &[u8]) -> u8 {
                    u8::from_le(bytes[1])
                }
                fn parse_d(bytes: &[u8]) -> i8 {
                    i8::from_le_bytes([bytes[1]])
                }

                match (x, z, y, q, p) {
                    (0, 0, 1, _, _) => {
                        let nn = parse_nn(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 20,
                            itype: InstructionType::Mem(
                                MemoryInstruction::LoadSPIntoConstantAddress(nn),
                            ),
                        })
                    }
                    (0, 1, _, 0, _) => {
                        let nn = parse_nn(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 20,
                            itype: InstructionType::Mem(MemoryInstruction::LoadImmediate16(
                                RP_TABLE[p as usize],
                                nn,
                            )),
                        })
                    }
                    (0, 0, 3, _, _) => {
                        let d = parse_d(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 12,
                            itype: InstructionType::Jump(JumpInstruction::JumpRelative(d)),
                        })
                    }
                    (0, 0, 4 | 5 | 6 | 7, _, _) => {
                        let d = parse_d(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 64, // MAGIC NUMBER
                            itype: InstructionType::Jump(JumpInstruction::JumpRelativeConditional(
                                CC_TABLE[(y - 4) as usize],
                                d,
                            )),
                        })
                    }
                    (0, 1, _, 1, _) => {
                        let nn = parse_nn(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 12,
                            itype: InstructionType::Mem(MemoryInstruction::LoadImmediate16(
                                RP_TABLE[p as usize],
                                nn,
                            )),
                        })
                    }
                    (0, 6, _, _, _) => {
                        let n = parse_n(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 8,
                            itype: InstructionType::Mem(MemoryInstruction::LoadImmediate8(
                                R_TABLE[y as usize],
                                n,
                            )),
                        })
                    }
                    (3, 0, 4, _, _) => {
                        let n = parse_n(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 12,
                            itype: InstructionType::Mem(MemoryInstruction::StorePortN(n)),
                        })
                    }
                    (3, 0, 5, _, _) => {
                        let d = parse_d(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 16,
                            itype: InstructionType::Arithmetic(
                                ArithmeticInstruction::ADDSPRelative(d),
                            ),
                        })
                    }
                    (3, 0, 6, _, _) => {
                        let n = parse_n(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 12,
                            itype: InstructionType::Mem(MemoryInstruction::LoadPortN(n)),
                        })
                    }
                    (3, 0, 7, _, _) => {
                        let d = parse_d(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 255, // UNKNOWN => MAGIC NUMBER
                            itype: InstructionType::Mem(MemoryInstruction::LoadHLFromSPOffset(d)),
                        })
                    }
                    (3, 2, 0 | 1 | 2 | 3 | 5 | 7, _, _) => {
                        let nn = parse_nn(bytes);
                        if y <= 3 {
                            PRes::Instruction(Instruction {
                                cycles: 64, // MAGIC NUMBER
                                itype: InstructionType::Jump(
                                    JumpInstruction::JumpConstantConditional(
                                        CC_TABLE[y as usize],
                                        nn,
                                    ),
                                ),
                            })
                        } else if y == 5 {
                            PRes::Instruction(Instruction {
                                cycles: 16,
                                itype: InstructionType::Mem(
                                    MemoryInstruction::LoadIndirectConstantFromA(nn),
                                ),
                            })
                        } else {
                            PRes::Instruction(Instruction {
                                cycles: 16,
                                itype: InstructionType::Mem(
                                    MemoryInstruction::LoadIndirectConstantToA(nn),
                                ),
                            })
                        }
                    }
                    (3, 3, 0, _, _) => {
                        let nn = parse_nn(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 16,
                            itype: InstructionType::Jump(JumpInstruction::JumpConstant(nn)),
                        })
                    }
                    (3, 4, 0 | 1 | 2 | 3, _, _) => {
                        let nn = parse_nn(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 64, // MAGIC NUMBER
                            itype: InstructionType::Jump(JumpInstruction::CallConstantConditional(
                                CC_TABLE[y as usize],
                                nn,
                            )),
                        })
                    }
                    (3, 5, _, 1, 0) => {
                        let nn = parse_nn(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 24,
                            itype: InstructionType::Jump(JumpInstruction::CallConstant(nn)),
                        })
                    }
                    (3, 6, _, _, _) => {
                        let n = parse_n(bytes);

                        PRes::Instruction(Instruction {
                            cycles: 8,
                            itype: InstructionType::Arithmetic(ArithmeticInstruction::ALUImediate(
                                ALU_TABLE[y as usize],
                                n,
                            )),
                        })
                    }
                    (_, _, _, _, _) => {
                        eprintln!("Bytes: {:?}", bytes.iter().map(|x| format!("{:02x}", x)).collect::<Vec<String>>());
                        eprintln!("decode opcode single: x: {x}, y: {y}, z: {z}, p: {p}, q: {q}");
                        panic!("This Should be unreachable, Instruction decode, further bytes")
                    }
                }
            }
            _ => panic!("No Clue"),
        }
    }
}

#[cfg(test)]
mod test {
    pub const BOOT_ROM_GB: [u8; 256] = [
        0x31, 0xfe, 0xff, 0xaf, 0x21, 0xff, 0x9f, 0x32, 0xcb, 0x7c, 0x20, 0xfb, 0x21, 0x26, 0xff,
        0x0e, 0x11, 0x3e, 0x80, 0x32, 0xe2, 0x0c, 0x3e, 0xf3, 0xe2, 0x32, 0x3e, 0x77, 0x77, 0x3e,
        0xfc, 0xe0, 0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1a, 0xcd, 0x95, 0x00, 0xcd, 0x96,
        0x00, 0x13, 0x7b, 0xfe, 0x34, 0x20, 0xf3, 0x11, 0xd8, 0x00, 0x06, 0x08, 0x1a, 0x13, 0x22,
        0x23, 0x05, 0x20, 0xf9, 0x3e, 0x19, 0xea, 0x10, 0x99, 0x21, 0x2f, 0x99, 0x0e, 0x0c, 0x3d,
        0x28, 0x08, 0x32, 0x0d, 0x20, 0xf9, 0x2e, 0x0f, 0x18, 0xf3, 0x67, 0x3e, 0x64, 0x57, 0xe0,
        0x42, 0x3e, 0x91, 0xe0, 0x40, 0x04, 0x1e, 0x02, 0x0e, 0x0c, 0xf0, 0x44, 0xfe, 0x90, 0x20,
        0xfa, 0x0d, 0x20, 0xf7, 0x1d, 0x20, 0xf2, 0x0e, 0x13, 0x24, 0x7c, 0x1e, 0x83, 0xfe, 0x62,
        0x28, 0x06, 0x1e, 0xc1, 0xfe, 0x64, 0x20, 0x06, 0x7b, 0xe2, 0x0c, 0x3e, 0x87, 0xe2, 0xf0,
        0x42, 0x90, 0xe0, 0x42, 0x15, 0x20, 0xd2, 0x05, 0x20, 0x4f, 0x16, 0x20, 0x18, 0xcb, 0x4f,
        0x06, 0x04, 0xc5, 0xcb, 0x11, 0x17, 0xc1, 0xcb, 0x11, 0x17, 0x05, 0x20, 0xf5, 0x22, 0x23,
        0x22, 0x23, 0xc9, 0xce, 0xed, 0x66, 0x66, 0xcc, 0x0d, 0x00, 0x0b, 0x03, 0x73, 0x00, 0x83,
        0x00, 0x0c, 0x00, 0x0d, 0x00, 0x08, 0x11, 0x1f, 0x88, 0x89, 0x00, 0x0e, 0xdc, 0xcc, 0x6e,
        0xe6, 0xdd, 0xdd, 0xd9, 0x99, 0xbb, 0xbb, 0x67, 0x63, 0x6e, 0x0e, 0xec, 0xcc, 0xdd, 0xdc,
        0x99, 0x9f, 0xbb, 0xb9, 0x33, 0x3e, 0x3c, 0x42, 0xb9, 0xa5, 0xb9, 0xa5, 0x42, 0x3c, 0x21,
        0x04, 0x01, 0x11, 0xa8, 0x00, 0x1a, 0x13, 0xbe, 0x20, 0xfe, 0x23, 0x7d, 0xfe, 0x34, 0x20,
        0xf5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xfb, 0x86, 0x20, 0xfe, 0x3e, 0x01, 0xe0,
        0x50,
    ];
    #[test]
    fn decode_boot_rom() {
        let mut decoded: Vec<InstructionType> = vec![];
        let mut idx = 0;
        // 0xa7 because NintendoLogo starts at a8 and NintendoLogo does not contain instructions :)
        while idx < 0xa8 {
            match Instruction::parse_from_bytes(&BOOT_ROM_GB[idx..idx + 1]) {
                PRes::NeedMoreBytes => {
                    let instr_res =
                        Instruction::parse_from_bytes(&BOOT_ROM_GB[idx..idx + 1_usize + 1]);
                    match instr_res {
                        PRes::Instruction(instr) => {
                            println!("CB Instruction at byte {:x} : {:?}", idx, instr.itype);
                            idx += 1;
                            decoded.push(instr.itype);
                        }
                        _ => panic!(),
                    }
                    idx += 1;
                }
                PRes::NeedMoreBytesSpecific(n_bytes) => {
                    let instr_res = Instruction::parse_from_bytes(
                        &BOOT_ROM_GB[idx..idx + n_bytes as usize + 1],
                    );
                    match instr_res {
                        PRes::Instruction(instr) => {
                            println!("Instruction at byte {:x} : {:?}", idx, instr.itype);
                            idx += 1;
                            decoded.push(instr.itype);
                        }
                        _ => panic!(),
                    }
                    idx += n_bytes as usize;
                }
                PRes::Instruction(instr) => {
                    println!("Instruction at byte {:x} : {:?}", idx, instr.itype);
                    idx += 1;
                    decoded.push(instr.itype);
                }
            }
        }

        use super::ALUOpTypes::*;
        use super::ArithmeticInstruction::*;
        use super::InstructionType::*;
        use super::JumpCondition::*;
        use super::JumpInstruction::*;
        use super::Location16Bit::*;
        use super::Location8Bit::*;
        use super::MemoryInstruction::*;
        use super::ShiftRotateInstruction::*;
        use super::ShiftRotateType::*;
        use super::SingleBitInstruction::*;
        use super::*;

        let solution_decoded_till_a8: [InstructionType; 101] = [
            Mem(LoadImmediate16(SP, 0xfffe)),
            Arithmetic(ALUReg8(XOR, A)),
            Mem(LoadImmediate16(HL, 0x9fff)),
            Mem(LoadIndirectFromA(HL, false, true)),
            SingleBit(TestBit(7, H)),
            Jump(JumpRelativeConditional(NZ, -5)),
            Mem(LoadImmediate16(HL, 0xff26)),
            Mem(LoadImmediate8(Location8Bit::C, 17)),
            Mem(LoadImmediate8(A, 128)),
            Mem(LoadIndirectFromA(HL, false, true)),
            Mem(StorePortC),
            Arithmetic(INC8(Location8Bit::C)),
            Mem(LoadImmediate8(A, 0xf3)),
            Mem(StorePortC),
            Mem(LoadIndirectFromA(HL, false, true)),
            Mem(LoadImmediate8(A, 0x77)),
            Mem(Load(Indirect, A)),
            Mem(LoadImmediate8(A, 0xfc)),
            Mem(StorePortN(0x47)), // 0xFF47 = 0xFF00 + 71?
            Mem(LoadImmediate16(DE, 0x0104)),
            Mem(LoadImmediate16(HL, 0x8010)),
            Mem(LoadIndirectToA(DE, false, false)),
            Jump(CallConstant(0x0095)),
            Jump(CallConstant(0x0096)),
            Arithmetic(INC16(DE)),
            Mem(Load(A, E)),
            Arithmetic(ALUImediate(CP, 0x34)),
            Jump(JumpRelativeConditional(NZ, -0x0d)),
            Mem(LoadImmediate16(DE, 0x00d8)),
            Mem(LoadImmediate8(B, 0x08)),
            Mem(LoadIndirectToA(DE, false, false)),
            Arithmetic(INC16(DE)),
            Mem(LoadIndirectFromA(HL, true, false)),
            Arithmetic(INC16(HL)),
            Arithmetic(DEC8(B)),
            Jump(JumpRelativeConditional(NZ, -0x07)),
            Mem(LoadImmediate8(A, 0x19)),
            Mem(LoadIndirectConstantFromA(0x9910)),
            Mem(LoadImmediate16(HL, 0x992f)),
            Mem(LoadImmediate8(Location8Bit::C, 0x0c)),
            Arithmetic(DEC8(A)),
            Jump(JumpRelativeConditional(Z, 8)),
            Mem(LoadIndirectFromA(HL, false, true)),
            Arithmetic(DEC8(Location8Bit::C)),
            Jump(JumpRelativeConditional(NZ, -7)),
            Mem(LoadImmediate8(L, 0x0f)),
            Jump(JumpRelative(-0x0d)),
            Mem(Load(H, A)),
            Mem(LoadImmediate8(A, 0x64)),
            Mem(Load(D, A)),
            Mem(StorePortN(0x42)),
            Mem(LoadImmediate8(A, 0x91)),
            Mem(StorePortN(0x40)),
            Arithmetic(INC8(B)),
            Mem(LoadImmediate8(E, 0x02)),
            Mem(LoadImmediate8(Location8Bit::C, 0x0c)),
            Mem(LoadPortN(0x44)),
            Arithmetic(ALUImediate(CP, 0x90)),
            Jump(JumpRelativeConditional(NZ, -6)),
            Arithmetic(DEC8(Location8Bit::C)),
            Jump(JumpRelativeConditional(NZ, -9)),
            Arithmetic(DEC8(E)),
            Jump(JumpRelativeConditional(NZ, -0x0e)),
            Mem(LoadImmediate8(Location8Bit::C, 0x13)),
            Arithmetic(INC8(H)),
            Mem(Load(A, H)),
            Mem(LoadImmediate8(E, 0x83)),
            Arithmetic(ALUImediate(CP, 0x62)),
            Jump(JumpRelativeConditional(Z, 6)),
            Mem(LoadImmediate8(E, 0xc1)),
            Arithmetic(ALUImediate(CP, 0x64)),
            Jump(JumpRelativeConditional(NZ, 6)),
            Mem(Load(A, E)),
            Mem(StorePortC),
            Arithmetic(INC8(Location8Bit::C)),
            Mem(LoadImmediate8(A, 0x87)),
            Mem(StorePortC),
            Mem(LoadPortN(0x42)),
            Arithmetic(ALUReg8(SUB, B)),
            Mem(StorePortN(0x42)),
            Arithmetic(DEC8(D)),
            Jump(JumpRelativeConditional(NZ, -0x2e)),
            Arithmetic(DEC8(B)),
            Jump(JumpRelativeConditional(NZ, 0x4f)),
            Mem(LoadImmediate8(D, 0x20)),
            Jump(JumpRelative(-0x35)),
            Mem(Load(Location8Bit::C, A)),
            Mem(LoadImmediate8(B, 4)),
            Mem(Push(BC)),
            InstructionType::ShiftRotate(ShiftRotateInstruction::ShiftRotate(RL, Location8Bit::C)),
            InstructionType::ShiftRotate(RLA),
            Mem(Pop(BC)),
            InstructionType::ShiftRotate(ShiftRotateInstruction::ShiftRotate(RL, Location8Bit::C)),
            InstructionType::ShiftRotate(ShiftRotateInstruction::RLA),
            Arithmetic(DEC8(B)),
            Jump(JumpRelativeConditional(NZ, -11)),
            Mem(LoadIndirectFromA(HL, true, false)),
            Arithmetic(INC16(HL)),
            Mem(LoadIndirectFromA(HL, true, false)),
            Arithmetic(INC16(HL)),
            Jump(Return),
        ];

        assert_eq!(decoded, solution_decoded_till_a8);
    }
}
