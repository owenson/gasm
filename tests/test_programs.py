"""Whole-program golden tests: each tests/programs/NAME.asm must assemble to NAME.bin.

boot.bin was produced by NASM; pmode.bin (gasm dialect: desc/tss/colonless
labels) was verified against an equivalent hand-translated NASM source.
"""

import glob
import os
import tempfile
import unittest

from tests.gasmtest import ROOT, NASM, nasm_assemble, run_gasm_file

PROGRAMS = os.path.join(ROOT, "tests", "programs")
NASM_COMPATIBLE = {"boot.asm"}


class TestPrograms(unittest.TestCase):
    def test_programs_present(self):
        self.assertTrue(glob.glob(os.path.join(PROGRAMS, "*.asm")))

    def test_golden_outputs(self):
        for src in sorted(glob.glob(os.path.join(PROGRAMS, "*.asm"))):
            name = os.path.basename(src)
            with self.subTest(program=name):
                with open(src[:-4] + ".bin", "rb") as f:
                    expected = f.read()
                with tempfile.TemporaryDirectory() as d:
                    out = os.path.join(d, "out.bin")
                    p = run_gasm_file(src, out, cwd=PROGRAMS)
                    self.assertEqual(p.returncode, 0, p.stderr)
                    with open(out, "rb") as f:
                        self.assertEqual(f.read().hex(), expected.hex())

    @unittest.skipUnless(NASM, "nasm not installed")
    def test_nasm_compatible_programs_match_nasm(self):
        for name in sorted(NASM_COMPATIBLE):
            with self.subTest(program=name):
                with open(os.path.join(PROGRAMS, name)) as f:
                    source = f.read()
                with open(os.path.join(PROGRAMS, name[:-4] + ".bin"), "rb") as f:
                    self.assertEqual(nasm_assemble(source), f.read())

    def test_boot_sector_signature(self):
        with open(os.path.join(PROGRAMS, "boot.bin"), "rb") as f:
            data = f.read()
        self.assertEqual(len(data), 512)
        self.assertEqual(data[510:], b"\x55\xaa")


if __name__ == "__main__":
    unittest.main()
