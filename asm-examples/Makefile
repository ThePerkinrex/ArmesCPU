ASM := cargo run --manifest-path ../Cargo.toml -p assembler --
LD := cargo run --manifest-path ../Cargo.toml -p linker --
EMULATOR := cargo run --manifest-path ../Cargo.toml -p linker --

multifile_test/main.elf: multifile_test/main.s
	$(ASM) -o multifile_test/main.elf multifile_test/main.s

multifile_test/lib.elf: multifile_test/lib.s
	$(ASM) -o multifile_test/lib.elf multifile_test/lib.s

multifile_test/linked.elf: multifile_test/main.elf multifile_test/lib.elf
	$(LD) -e multifile_test/linked.elf multifile_test/main.elf multifile_test/lib.elf

multifile_test/linked.o: multifile_test/main.elf multifile_test/lib.elf
	$(LD) -o multifile_test/linked.o multifile_test/main.elf multifile_test/lib.elf

build_multifile: multifile_test/linked.o multifile_test/linked.elf
run_multifile: multifile_test/linked.o
	$(EMULATOR) multifile_test/linke.o

