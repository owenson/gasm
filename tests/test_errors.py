"""Error reporting, command line behaviour and robustness."""

import os
import random
import subprocess
import tempfile
import unittest

from tests.gasmtest import GASM, assemble, run_gasm_file


class TestErrors(unittest.TestCase):
    CASES = [
        # (source, line, message fragment)
        ("nop\nmov ax, undefined_label\n", 2, "undefined label/identifier 'undefined_label'"),
        ("foo:\nfoo:\n", 2, "duplicate label 'foo'"),
        ("a:\n.x: nop\n.x: nop\n", 3, "duplicate label 'a.x'"),
        ("mov al, bx\n", 1, "invalid combination of opcode and operands"),
        ("mov al, 300\n", 1, "immediate value out of range"),
        ("add ax, 0x12345\n", 1, "immediate value out of range"),
        ("int 256\n", 1, "immediate value out of range"),
        ("jmp short far_label\ntimes 200 nop\nfar_label:\n", 1, "short jump out of range"),
        ("loop target\ntimes 300 nop\ntarget:\n", 1, "jump target out of range"),
        ("imul ax, bx, 5, 6\n", 1, "too many operands"),
        ("mov ax, bx, cx\n", 1, "too many operands for 'mov'"),
        ("push\n", 1, "'push' requires operands"),
        ("mov ax, [bx+cx]\n", 1, "invalid 16-bit effective address '[bx+cx]'"),
        ("mov ax, [bx+eax]\n", 1, "cannot mix 16 and 32-bit registers"),
        ("mov ax, [bx*2]\n", 1, "scaled index not allowed with 16-bit addressing"),
        ("mov eax, [esp*2]\n", 1, "esp cannot be used as an index register"),
        ("mov eax, [eax*3]\n", 1, "invalid scale factor 3"),
        ("mov eax, [eax+ebx+ecx]\n", 1, "too many registers in memory operand"),
        ("mov ax, [al]\n", 1, "8-bit register not allowed in memory operand"),
        ("mov ax, [bx-si]\n", 1, "invalid effective address"),
        ("mov ax, [bx\n", 1, "missing ']'"),
        ("mov ax, []\n", 1, "empty memory operand"),
        ("mov ax, [bx+0x10000]\n", 1, "out of range for 16-bit addressing"),
        ("[foo 1]\n", 1, "unknown directive '[foo]'"),
        ("[bits 64]\n", 1, "invalid BITS operand"),
        ("%include \"does_not_exist.inc\"\n", 1, "unable to open 'does_not_exist.inc'"),
        ("%define X 1\n", 1, "unknown preprocessor directive '%define'"),
        ("times -1 db 0\n", 1, "TIMES count is negative"),
        ("times 5\n", 1, "TIMES requires an instruction or data"),
        ("resb -4\n", 1, "negative reserve count"),
        ("db 1/0\n", 1, "division by zero"),
        ("db 5 %% 0\n", 1, "division by zero"),
        ("mvo ax, bx\n", 1, "unknown instruction 'mvo'"),
        ("db 0x\n", 1, "invalid number '0x'"),
        ("db 12z\n", 1, "invalid number '12z'"),
        ("db (1+2\n", 1, "missing ')'"),
        ("db 1 +\n", 1, "expression expected"),
        ("db 1 2\n", 1, "unexpected '2' in expression"),
        ("db 'abc\n", 1, "unterminated string"),
        ("db 1,,2\n", 1, "empty operand"),
        ("db\n", 1, "data expected"),
        ("mov ax, ax + 1\n", 1, "register 'ax' not allowed in expression"),
        ("pop cs\n", 1, "pop cs is not a valid instruction"),
        ("ax: nop\n", 1, "invalid label name 'ax'"),
        ("x equ\n", 1, "EQU requires a value"),
        ("desc 1, 2\n", 1, "desc requires 3 operands"),
        ("align 3\n", 1, "alignment must be a power of two"),
        ("jmp far 0x1234\n", 1, "invalid use of 'far'"),
        ("mov byte ax, 1\n", 1, "register size does not match size keyword"),
        ("mov byte [bx], ax\n", 1, "invalid combination of opcode and operands"),
        ("imul ax, ebx, 5\n", 1, "operand size mismatch"),
        ("org later\nlater:\n", 1, "ORG value must not use forward references"),
        ("nop\nnop\n" + "x" * 5000 + "\n", 3, "line too long"),
        ("l" * 200 + ": nop\n", 1, "too long"),
    ]

    def test_error_messages(self):
        for source, line, fragment in self.CASES:
            with self.subTest(source=source[:60]):
                res = assemble(source)
                self.assertEqual(res.returncode, 1, f"expected failure, got {res}")
                self.assertIn(f"test.asm:{line}: error: ", res.stderr)
                self.assertIn(fragment, res.stderr)
                self.assertIsNone(res.output, "no output file should be written on error")

    def test_error_in_included_file_reports_that_file(self):
        res = assemble('nop\n%include "sub.inc"\n', files={"sub.inc": "nop\nbogus_instruction ax\n"})
        self.assertEqual(res.returncode, 1)
        self.assertIn("sub.inc:2: error: unknown instruction 'bogus_instruction'", res.stderr)

    def test_line_numbers_restored_after_include(self):
        res = assemble('%include "ok.inc"\nnop\nmov al, bx\n', files={"ok.inc": "nop\nnop\nnop\nnop\n"})
        self.assertIn("test.asm:3: error:", res.stderr)

    def test_recursive_include(self):
        res = assemble('%include "test.asm"\n')
        self.assertEqual(res.returncode, 1)
        self.assertIn("includes nested too deeply", res.stderr)

    def test_existing_output_not_clobbered_on_error(self):
        with tempfile.TemporaryDirectory() as d:
            src, out = os.path.join(d, "a.asm"), os.path.join(d, "a.bin")
            with open(out, "wb") as f:
                f.write(b"previous")
            with open(src, "w") as f:
                f.write("mov al, bx\n")
            p = run_gasm_file(src)
            self.assertEqual(p.returncode, 1)
            with open(out, "rb") as f:
                self.assertEqual(f.read(), b"previous")


class TestCommandLine(unittest.TestCase):
    def run_in(self, d, args):
        p = subprocess.run([GASM] + args, capture_output=True, text=True, cwd=d)
        self.assertGreaterEqual(p.returncode, 0, "killed by a signal")
        return p

    def test_usage(self):
        with tempfile.TemporaryDirectory() as d:
            p = self.run_in(d, [])
            self.assertEqual(p.returncode, 1)
            self.assertIn("Usage: gasm", p.stdout)
            p = self.run_in(d, ["a", "b", "c"])
            self.assertEqual(p.returncode, 1)

    def test_missing_source(self):
        with tempfile.TemporaryDirectory() as d:
            p = self.run_in(d, ["-q", "nope.asm"])
            self.assertEqual(p.returncode, 1)
            self.assertIn("unable to open 'nope.asm'", p.stderr)

    def test_default_output_name(self):
        # the extension of the file name is replaced, even when directories contain dots
        with tempfile.TemporaryDirectory() as d:
            os.makedirs(os.path.join(d, "dir.v1"))
            for rel, expected in [("prog.asm", "prog.bin"), ("dir.v1/boot.s", "dir.v1/boot.bin"),
                                  ("noext", "noext.bin"), ("./x.y.asm", "x.y.bin"), (".hidden", ".hidden.bin")]:
                with open(os.path.join(d, rel), "w") as f:
                    f.write("nop\n")
                p = self.run_in(d, ["-q", rel])
                self.assertEqual(p.returncode, 0, p.stderr)
                with open(os.path.join(d, expected), "rb") as f:
                    self.assertEqual(f.read(), b"\x90", rel)

    def test_explicit_output_name(self):
        with tempfile.TemporaryDirectory() as d:
            with open(os.path.join(d, "a.asm"), "w") as f:
                f.write("ret\n")
            p = self.run_in(d, ["-q", "a.asm", "out.img"])
            self.assertEqual(p.returncode, 0, p.stderr)
            with open(os.path.join(d, "out.img"), "rb") as f:
                self.assertEqual(f.read(), b"\xc3")

    def test_banner_unless_quiet(self):
        with tempfile.TemporaryDirectory() as d:
            with open(os.path.join(d, "a.asm"), "w") as f:
                f.write("ret\n")
            self.assertIn("GASM", self.run_in(d, ["a.asm"]).stdout)
            self.assertEqual(self.run_in(d, ["-q", "a.asm"]).stdout, "")

    def test_crlf_source(self):
        with tempfile.TemporaryDirectory() as d:
            with open(os.path.join(d, "a.asm"), "wb") as f:
                f.write(b"start:\r\n  mov ax, 1234h\r\n  jmp start\r\n")
            p = self.run_in(d, ["-q", "a.asm"])
            self.assertEqual(p.returncode, 0, p.stderr)
            with open(os.path.join(d, "a.bin"), "rb") as f:
                self.assertEqual(f.read().hex(), "b83412ebfb")


class TestRobustness(unittest.TestCase):
    """Random and hostile input must produce a clean result or a clean error, never a crash."""

    TOKENS = ["mov", "add", "jmp", "push", "db", "dw", "times", "equ", "[", "]", "+", "-", "*", ",", ":", "'",
              "\"", "(", ")", "ax", "al", "eax", "bx", "bp", "esp", "es", "cr0", "byte", "word", "dword", "far",
              "short", "0x10", "10h", "5", "-1", "$", "$$", "label", ".loc", "%include", ";", "<<", "/", "%",
              "[bits", "32]", "org", "tss", "desc", "rep", "lock", "imul", "0", "99999999999999999999"]

    def check(self, source):
        res = assemble(source, timeout=60)
        self.assertIn(res.returncode, (0, 1), f"crash on {source!r}: {res}")
        if res.returncode == 1:
            self.assertIn("error", res.stderr, source)

    def test_random_token_soup(self):
        rng = random.Random(1234)
        for _ in range(300):
            lines = [" ".join(rng.choice(self.TOKENS) for _ in range(rng.randint(1, 7))) for _ in range(rng.randint(1, 4))]
            self.check("label: nop\n" + "\n".join(lines) + "\n")

    def test_random_bytes(self):
        rng = random.Random(99)
        for _ in range(100):
            data = bytes(rng.randint(1, 255) for _ in range(rng.randint(1, 200)))
            self.check(data.decode("latin-1"))

    def test_huge_values(self):
        for src in ["times 0x7fffffffffffffff db 0\n", "times 1<<62 nop\n", "resd 0x4000000000000000\n",
                    "db 1 << 70\n", "dq 1\n", "mov eax, [0xffffffffffff]\n", "align 0x8000000000000000\n",
                    "db 0x7fffffffffffffff * 2\n", "db -(0x8000000000000000)\n", "db 0x7fffffffffffffff + 1\n",
                    "db 0x8000000000000000 // -1\n", "db 0x8000000000000000 %% -1\n", "db -0x8000000000000000 - 1\n",
                    "db ~0 << 63, 1 << -1, 1 >> 64\n", "jmp 0x7fffffffffffffff\n", "jmp -0x8000000000000000\n",
                    "org 0xffffffffffffffff\n", "mov ax, [bx+0x7fffffffffffffff]\n", "times 1<<40 align 1\n"]:
            with self.subTest(src=src):
                self.check(src)

    def test_large_times(self):
        res = assemble("times 100000 db 0x41\n")
        self.assertTrue(res.ok, res.stderr)
        self.assertEqual(res.output, b"A" * 100000)

    def test_long_operands(self):
        self.check("mov ax, " + "+".join(["1"] * 1500) + "\n")
        self.check("mov ax, [" + "+".join(["bx"] * 3) + "]\n")
        self.check("db " + "(" * 1000 + "1" + ")" * 1000 + "\n")


if __name__ == "__main__":
    unittest.main()
