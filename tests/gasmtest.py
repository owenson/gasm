"""Shared helpers for the gasm test suite.

The assembler binary is taken from the GASM environment variable (default: ./gasm
in the repository root). NASM/ndisasm are optional; tests that need them are
skipped when they are not installed.
"""

import os
import re
import shutil
import subprocess
import tempfile
import unittest

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
GASM = os.path.abspath(os.environ.get("GASM", os.path.join(ROOT, "gasm")))
NASM = shutil.which("nasm")
NDISASM = shutil.which("ndisasm")

needs_nasm = unittest.skipUnless(NASM and NDISASM, "nasm/ndisasm not installed")


class Result:
    def __init__(self, returncode, output, stdout, stderr):
        self.returncode = returncode
        self.output = output          # bytes, or None if no output file was written
        self.stdout = stdout
        self.stderr = stderr

    @property
    def ok(self):
        return self.returncode == 0

    def __repr__(self):
        out = self.output.hex() if self.output is not None else None
        return f"Result(rc={self.returncode}, output={out}, stderr={self.stderr!r})"


def run_gasm_file(path, outpath=None, cwd=None, extra_args=(), timeout=30):
    args = [GASM, "-q", *extra_args, path]
    if outpath is not None:
        args.append(outpath)
    # Sanitizers exit with status 1 by default, like a normal assembly error; make them distinct
    env = dict(os.environ)
    env.setdefault("ASAN_OPTIONS", "exitcode=86")
    env.setdefault("UBSAN_OPTIONS", "exitcode=86:print_stacktrace=1")
    p = subprocess.run(args, capture_output=True, text=True, cwd=cwd, timeout=timeout, env=env)
    if p.returncode < 0:
        raise AssertionError(f"gasm killed by signal {-p.returncode}: {p.stderr}")
    if p.returncode == 86 or "runtime error:" in p.stderr or "Sanitizer" in p.stderr:
        raise AssertionError(f"sanitizer error:\n{p.stderr}")
    return p


def assemble(source, files=None, timeout=30):
    """Assembles source text with gasm in a scratch directory.

    files: optional {name: text} of extra files (for %include tests).
    """
    with tempfile.TemporaryDirectory() as d:
        src = os.path.join(d, "test.asm")
        out = os.path.join(d, "test.bin")
        with open(src, "w") as f:
            f.write(source)
        for name, text in (files or {}).items():
            p = os.path.join(d, name)
            os.makedirs(os.path.dirname(p), exist_ok=True)
            with open(p, "w") as f:
                f.write(text)
        p = run_gasm_file(src, out, cwd=d, timeout=timeout)
        data = None
        if os.path.exists(out):
            with open(out, "rb") as f:
                data = f.read()
        return Result(p.returncode, data, p.stdout, p.stderr)


def nasm_assemble(source):
    """Assembles source text with NASM (-f bin). Returns bytes or None on error."""
    with tempfile.TemporaryDirectory() as d:
        src = os.path.join(d, "t.asm")
        out = os.path.join(d, "t.bin")
        with open(src, "w") as f:
            f.write(source)
        p = subprocess.run([NASM, "-f", "bin", "-w-all", "-o", out, src], capture_output=True, text=True)
        if p.returncode != 0:
            return None
        with open(out, "rb") as f:
            return f.read()


def disassemble(data, bits):
    with tempfile.TemporaryDirectory() as d:
        path = os.path.join(d, "x.bin")
        with open(path, "wb") as f:
            f.write(data)
        p = subprocess.run([NDISASM, "-b", str(bits), path], capture_output=True, text=True)
        return [" ".join(line.split()[2:]) for line in p.stdout.splitlines()]


_SIGNED_IMM = re.compile(r"\b(byte|word|dword) ([+-])0x([0-9a-f]+)")


def _operand_width(line, bits):
    if re.search(r"\b(e[a-ds][xip]|esp|ebp|esi|edi)\b|\bdword\b", line.split(",")[0]):
        return 32
    if re.search(r"\b([a-d]x|sp|bp|si|di)\b|\bword\b", line.split(",")[0]):
        return 16
    if line.startswith("o16"):
        return 16
    if line.startswith("o32"):
        return 32
    return bits


def normalize_disassembly(lines, bits):
    """Canonicalises ndisasm output so equivalent encodings compare equal
    (e.g. sign-extended imm8 vs imm16, shift by 1 forms, short/near jumps)."""
    out = []
    for line in lines:
        width = _operand_width(line, bits)

        def fix(m):
            v = int(m.group(3), 16)
            if m.group(2) == "-":
                v = -v
            return hex(v & ((1 << width) - 1))

        line = _SIGNED_IMM.sub(fix, line)
        line = re.sub(r",byte (0x[0-9a-f]+)$", r",\1", line)
        line = re.sub(r",1$", ",0x1", line)
        line = re.sub(r"\b(short|near) ", "", line)
        line = line.replace("+0x0]", "]")
        out.append(line)
    return out
