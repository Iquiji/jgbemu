
#[derive(Debug, Clone)]
pub enum Instruction{
    
    NOP,
    Unknown,
}

impl Instruction{
    /// Returns Instruction or needs more Bytes :)
    pub fn parse_from_bytes(bytes: &[u8]) -> Result<Instruction, ()>{
        // Prefixes:
        if [0xCB, 0xDD, 0xED, 0xFD].contains(bytes.last().unwrap()) {
            return Err(())
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
        let opcode_byte = bytes.last().unwrap();
        let x: u8 = (opcode_byte & 0b1100_0000) >> 6;
        let y: u8 = (opcode_byte & 0b0011_1000) >> 3;
        let z: u8 = opcode_byte & 0b0000_0111;
        let p: u8 = y >> 1;
        let q: u8 = y % 2;
        println!("x: {x}, y: {y}, z: {z}, p: {p}, q: {q}");


        Ok(Instruction::Unknown)
    }
}