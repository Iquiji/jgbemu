# JGBEmu (Jank Gameboy Emulator)

## Sources:

General Sources:

- https://gbdev.io/pandocs/
- https://github.com/Gekkio/mooneye-gb
- https://github.com/gbdev/awesome-gbdev#emulator-development

Decoding Instructions:

- https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
- https://izik1.github.io/gbops/

Boot ROM dissasembly:

- https://gist.github.com/knightsc/ab5ebda52045b87fa1772f5824717ddf


## Blargg Test Suite Status

[x] 01 - special

[ ] 02 - interrupts (Timing of Instructions not accurate, EI not working)

[x] 03 - op sp, hl 

[x] 04 - op r,imm

[x] 05 - op rp

[x] 06 - ld r,r

[x] 07 - jr, jp, call, ret, rst (jump location wrong, maybe lsb and msb switched up)

[x] 08 - misc instrs

[x] 09 - op r,r

[x] 10 - bit ops

[x] 11 - op a,(hl)