x86 Assembler by Dr Gareth Owen 

NASM compatible.
Open source licence.

## Building

    make            # builds ./gasm
    make test       # runs the test suite (Python 3; NASM/ndisasm optional)
    make test-asan  # runs the test suite against an AddressSanitizer/UBSan build

## Usage

    gasm [-q] source.asm [output.bin]

Produces a flat binary. Without an output name the source extension is replaced
with `.bin`. `-q` suppresses the banner. Errors are reported on stderr as
`file:line: error: message`, the exit status is 1 and no output file is written.

## Syntax

gasm accepts the NASM syntax for 16 and 32-bit integer code:

* `[BITS 16|32]` / `bits`, `[ORG n]` / `org`, `%include "file"`
* `db`/`dw`/`dd` (numbers, expressions, strings), `resb`/`resw`/`resd`,
  `times`, `align`/`alignb`, `equ`
* expressions with C precedence: `| ^ & << >> + - * / // % %% ~ ! ( )`,
  `$`, `$$`, character constants, and numbers written as `10`, `0x1F`, `1Fh`,
  `0b101`/`101b`, `17q`/`17o`
* memory operands `[base+index*scale+disp]` with segment overrides
  (`[es:bx]` or gasm's `es:[bx]`), `byte`/`word`/`dword` size keywords,
  `short`/`near`/`far`
* NASM style local labels (`.loop` belongs to the previous global label)

Branch sizes are chosen automatically (short when the target is in range).

gasm specific extensions and differences:

* `desc base, limit, flags` builds a GDT/IDT descriptor; the `D_*` constants
  (`D_CODE`, `D_DATA`, `D_READ`, `D_WRITE`, `D_BIG`, `D_BIG_LIM`, `D_INT`,
  `D_TRAP`, `D_TSS`, `D_DPL1..3`, `D_PRESENT`, `D_NOT_PRESENT`, ...) are predefined
* `name tss` reserves a 104 byte TSS and defines `name.esp0`, `name.ss0`, ...
* labels and mnemonics are case insensitive, and labels may omit the colon
* a memory operand without a size keyword takes its size from the other operand,
  or defaults to the current `BITS` size
* `ORG` adds to the current address
* out of range immediates are errors rather than warnings

## Tests

`tests/` contains a Python `unittest` suite:

* `test_encoding_nasm.py` assembles every form in the opcode tables (about
  3,300 cases per mode, 6,300 compared in total) with gasm and NASM and requires
  identical or equivalent output (skipped without NASM)
* `test_assembler.py` checks exact output for directives, expressions, labels,
  branch sizing, addressing modes and regressions for fixed bugs
* `test_errors.py` covers error messages, the command line, and fuzzed input
* `test_programs.py` checks the programs in `tests/programs` against golden binaries

Set `GASM=/path/to/gasm` to test another build.
