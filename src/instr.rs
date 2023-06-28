use core::panic;

/// Register or Indirect Memory access
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
    /// Magic numbers regarding cycle count:
    /// 64 ^= Conditional so we don't know
    /// 255 ^= indefinete
    /// 128 ^= until interupt
    pub cycles: u8,
    pub itype: InstructionType,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub enum ShiftRotateInstruction {
    ShiftRotate(ShiftRotateType, Location8Bit),
}

#[derive(Debug, Clone)]
pub enum SingleBitInstruction {
    /// u8 means which Bit
    TestBit(u8, Location8Bit),
    ResetBit(u8, Location8Bit),
    SetBit(u8, Location8Bit),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Copy)]
pub enum JumpCondition {
    NZ,
    Z,
    NC,
    C,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
                    (0, 7, _) => {
                        unimplemented!("Assorted operations on accumulator/flags");
                    }
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
                                itype: InstructionType::Mem(MemoryInstruction::Push(
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
                            itype: InstructionType::Mem(
                                MemoryInstruction::LoadImmediate16(RP_TABLE[p as usize], nn)
                            ),
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
                        panic!("This Should be unreachable, Instruction decode, further bytes")
                    }
                }
            }
            _ => panic!("No Clue"),
        }
    }
}

#[cfg(test)]
mod test{
    #[test]
    fn decode_boot_rom(){
        assert!(false)
    }
}