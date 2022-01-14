# ArmesCPU  
[![Rust](https://github.com/ThePerkinrex/ArmesCPU/actions/workflows/rust.yml/badge.svg)](https://github.com/ThePerkinrex/ArmesCPU/actions/workflows/rust.yml)

The third attempt at making a CPU emulator with a custom instruction set

It consists of 3 crates:
 * `asm-ir` contains the abstract representation of the instruction set as well as a binary serializer and deserializer
 * `armes-elf` contains an abstract representation of a linkable format with a serializer & deserializer
 * `assembler` contains the simple assembler
 * `linker` contains a linker for the ELF format
 * `emulator` contains the actual cpu emulator
