"""Differential tests: every form in gasm's opcode tables is assembled with both
gasm and NASM, in 16 and 32-bit mode, and the results must be identical (or
disassemble to the same instruction when the two pick equivalent encodings).

The instruction list is generated from gasm.h, so new table entries are tested
automatically.
"""

import itertools
import os
import re
import unittest
from concurrent.futures import ThreadPoolExecutor

from tests.gasmtest import (ROOT, NASM, NDISASM, assemble, disassemble, nasm_assemble,
                            needs_nasm, normalize_disassembly)

SAMPLES = {
    "NONE": [None],
    "REG8": ["cl", "bh"],
    "REG16": ["cx", "di"],
    "REG32": ["ecx", "edi"],
    "REGM8": ["dl", "byte [bx+si+4]", "byte [ebx+ecx*4+0x100]", "byte [0x1234]"],
    "REGM16": ["si", "word [bp-2]", "word [esp+8]", "word [bp]"],
    "REGM32": ["esi", "dword [di]", "dword [ebp]", "dword [eax*2+0x10]", "dword [0x1234]"],
    "IMM8": ["5", "-3", "0x7f"],
    "IMM16": ["0x1234", "-300"],
    "IMM32": ["0x12345678", "-100000"],
    "AL": ["al"],
    "AX": ["ax"],
    "EAX": ["eax"],
    "DX": ["dx"],
    "CL": ["cl"],
    "SREG": ["es", "ds", "fs"],
    "CREG": ["cr0", "cr3"],
    "DREG": ["dr1", "dr7"],
    "PTR16_PTR16": ["0x1234:0x5678"],
    "PTR16_PTR32": ["dword 0x1234:0x12345678"],
}
REL_TARGETS = ["$+0x10", "$-0x20", "$+0x400"]

ENTRY = re.compile(r'\{\s*0x[0-9A-Fa-f]+,\s*0x[0-9A-Fa-f]+,\s*0x[0-9A-Fa-f]+,\s*\d,\s*MODRM_OPCODE_\d,\s*'
                   r'EVALUATE_(\w+),\s*EVALUATE_(\w+),\s*(FORMAT_\w+)\s*,\s*"(\w+)"\s*\}')
FIXED = re.compile(r'\{\s*0x[0-9A-Fa-f]+,\s*0x[0-9A-Fa-f]+,\s*0x[0-9A-Fa-f]+,\s*\d,\s*"(\w+)",\s*\d+\s*\}')

# Extra hand written cases for forms that are not table driven
EXTRA = [
    "push es", "push cs", "push ss", "push ds", "push fs", "push gs",
    "pop es", "pop ss", "pop ds", "pop fs", "pop gs",
    "imul cx, dx, 5", "imul ecx, [bx], 1000", "imul ax, 200", "imul eax, -2",
    "shl ax, 1", "rcr byte [bx], 1", "sar dword [esi], 1",
    "mov al, [0x1234]", "mov [0x1234], ax", "mov eax, [0x1234]", "mov [es:0x10], al",
    "mov ax, [es:bx]", "mov al, [cs:si+2]", "add [fs:eax], ebx", "lea ax, [gs:bx]",
    "test bl, al", "test cx, [bx]", "test [bx], eax",
    "jmp far [bx]", "call far [ebx+4]",
    "rep movsb", "rep stosd", "repne scasb", "repe cmpsw", "lock add [bx], ax", "lock inc dword [eax]",
    "mov eax, [esp]", "mov eax, [esp+4]", "mov eax, [ebp]", "mov eax, [ebp+esi]", "mov eax, [esi+ebp]",
    "mov eax, [eax+esp]", "mov eax, [ebx*8]", "mov eax, [4*ebx+eax]", "mov eax, [eax+ebx*2-0x80]",
    "mov eax, [edx+0x7f]", "mov eax, [edx+0x80]", "mov eax, [edx-0x80]", "mov eax, [edx-0x81]",
    "mov ax, [bx+si]", "mov ax, [bx+di]", "mov ax, [bp+si]", "mov ax, [bp+di]", "mov ax, [si]",
    "mov ax, [di]", "mov ax, [bp]", "mov ax, [bx]", "mov ax, [si+bx]", "mov ax, [di+bp+0x1234]",
    "mov ax, [bx-1]", "mov ax, [bx+0x7f]", "mov ax, [bx+0x80]", "mov ax, [bx+(2*3)]",
    "jcxz $+0x10", "jecxz $+0x10", "loop $", "loope $-4", "loopnz $+0x20",
    "jmp short $+0x20", "jmp near $+0x20", "call $+0x1000",
]

# Known, intentional differences from NASM: (line, bits), bits None = both modes
SKIP = {
    # NASM picks "o16 push byte" (66 6A); gasm emits the equivalent "push word" (66 68 iw)
    ("push word 5", None),
    # In 16-bit mode NASM silently truncates these to a word push; gasm pushes the full dword
    ("push 0x12345678", 16),
    ("push -100000", 16),
}


def skipped(line, bits):
    return (line, None) in SKIP or (line, bits) in SKIP


def table_instructions():
    with open(os.path.join(ROOT, "gasm.h")) as f:
        header = f.read()
    dyn = header[header.index("dynamic_lookup[]"):]
    fixed = header[header.index("fixed_instruction_lookup[]"):header.index("dynamic_lookup[]")]

    lines = []
    for m in ENTRY.finditer(dyn):
        p1, p2, fmt, name = m.groups()
        rel = "REL" in fmt
        s1 = REL_TARGETS if rel else SAMPLES[p1]
        s2 = SAMPLES[p2]
        for a, b in itertools.product(s1, s2):
            ops = [x for x in (a, b) if x is not None]
            lines.append(f"{name} {', '.join(ops)}" if ops else name)
    for m in FIXED.finditer(fixed):
        lines.append(m.group(1))
    lines += EXTRA
    seen = set()
    return [l for l in lines if not (l in seen or seen.add(l))]


def check_line(line, bits):
    """Returns None if gasm agrees with NASM (or NASM rejects the line), else a message."""
    src = f"bits {bits}\n{line}\n"
    expected = nasm_assemble(src)
    if expected is None:
        return None     # not valid NASM; not a differential case
    res = assemble(src)
    if not res.ok:
        return f"[{bits}] {line!r}: gasm failed: {res.stderr.strip()} (nasm: {expected.hex()})"
    if res.output == expected:
        return None
    g = normalize_disassembly(disassemble(res.output, bits), bits)
    n = normalize_disassembly(disassemble(expected, bits), bits)
    if g == n:
        return None
    return f"[{bits}] {line!r}: gasm {res.output.hex()} {g} != nasm {expected.hex()} {n}"


@needs_nasm
class TestTableAgainstNasm(unittest.TestCase):
    results = {}

    @classmethod
    def setUpClass(cls):
        lines = table_instructions()
        jobs = [(l, b) for b in (16, 32) for l in lines if not skipped(l, b)]
        with ThreadPoolExecutor(max_workers=(os.cpu_count() or 2) * 2) as ex:
            for (l, b), r in zip(jobs, ex.map(lambda j: check_line(*j), jobs)):
                cls.results[(l, b)] = r
        cls.count = len(jobs)

    def test_generated_cases_exist(self):
        # guard against the table parser silently matching nothing
        self.assertGreater(self.count, 2000)

    def _check(self, bits):
        failures = [r for (l, b), r in sorted(self.results.items()) if b == bits and r]
        self.assertEqual(failures, [], f"{len(failures)} mismatches:\n" + "\n".join(failures[:50]))

    def test_bits16(self):
        self._check(16)

    def test_bits32(self):
        self._check(32)


if __name__ == "__main__":
    unittest.main()
