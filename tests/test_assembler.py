"""Exact-output tests for gasm's syntax, directives, labels and encodings.

Cases marked nasm=True use NASM-compatible syntax; when NASM is installed the
expected bytes are also checked against NASM's output so the expectations
cannot drift from the reference assembler.
"""

import unittest

from tests.gasmtest import NASM, NDISASM, assemble, disassemble, nasm_assemble


class AsmTestCase(unittest.TestCase):
    def assertAsm(self, source, expected_hex, nasm=False):
        expected = bytes.fromhex(expected_hex.replace(" ", "").replace("\n", ""))
        res = assemble(source)
        self.assertTrue(res.ok, f"gasm failed: {res.stderr}\nsource:\n{source}")
        self.assertEqual(res.output.hex(" "), expected.hex(" "), f"source:\n{source}")
        if nasm and NASM:
            self.assertEqual(nasm_assemble(source).hex(" "), expected.hex(" "), "expectation differs from NASM")
        return res.output


class TestNumbersAndExpressions(AsmTestCase):
    def test_number_formats(self):
        self.assertAsm("db 10, 10h, 0x10, 0Ah, 0FFh, 101b, 0b101, 17q, 17o, 10d, 1_000 & 0xFF, 'A', '0'\n",
                       "0a 10 10 0a ff 05 05 0f 0f 0a e8 41 30", nasm=True)

    def test_uppercase_hex_suffix(self):
        self.assertAsm("db 0FFH, 1AH\n", "ff 1a", nasm=True)

    def test_operator_precedence(self):
        # (NASM cannot parse "7%3" outside a macro, so these are checked by hand)
        self.assertAsm("db 2+3*4, (2+3)*4, 20-4-2, 64/4/2, 7 % 3, -1+2, -(1+2), ~0 & 0xF\n"
                       "db 1<<4|1, 0xF0>>4, 6&3, 6^3, 6|1, 2*-3, !0, !5\n",
                       "0e 14 0e 08 01 01 fd 0f 11 0f 02 05 07 fa 01 00")

    def test_signed_and_unsigned_division(self):
        self.assertAsm("dw -7/2, -7//2, -7 %% 3, 7//2\n", "fc ff fd ff ff ff 03 00")

    def test_character_constants(self):
        self.assertAsm("dw 'ab'\ndd 'abcd'\nmov ax, 'hi'\n", "61 62 61 62 63 64 b8 68 69", nasm=True)

    def test_dollar_and_dollar_dollar(self):
        self.assertAsm("org 0x100\nstart: dw $, $$, start\nmov ax, start\n", "00 01 00 01 00 01 b8 00 01", nasm=True)

    def test_dollar_in_times_is_line_start(self):
        self.assertAsm("nop\ntimes 3 jmp $\ntimes 2 dw $\nx: times 2 db $-x\n",
                       "90 eb fe eb fc eb fa 07 00 07 00 00 00", nasm=True)

    def test_negative_immediates(self):
        self.assertAsm("mov ax, -1\nadd bx, -2\nmov al, -128\nmov al, 255\n", "b8 ff ff 83 c3 fe b0 80 b0 ff", nasm=True)

    def test_label_arithmetic(self):
        self.assertAsm("a: nop\nb: nop\nnop\nc: db c-a, b-a, (c-a)*2\n", "90 90 90 03 01 06", nasm=True)


class TestLabels(AsmTestCase):
    def test_forward_references(self):
        self.assertAsm("mov ax, data\nmov bx, [data]\njmp end\ndata: dw 0x1234\nend: ret\n",
                       "b8 09 00 8b 1e 09 00 eb 02 34 12 c3", nasm=True)

    def test_local_labels(self):
        self.assertAsm("a:\n.l: jmp .l\nb:\n.l: jmp .l\njmp a.l\njmp b.l\n", "eb fe eb fe eb fa eb fa", nasm=True)

    def test_label_without_colon(self):
        self.assertAsm("start nop\nloop1 dec cx\njnz loop1\nvalue dw start\n", "90 49 75 fd 00 00")

    def test_label_case_insensitive(self):
        # gasm labels and mnemonics are case insensitive
        self.assertAsm("MOV AX, BX\nInt 21H\nJMP Label\nlabel:\n", "89 d8 cd 21 eb 00")

    def test_label_names_containing_keywords(self):
        # Original gasm treated these as EQU / TSS / "byte" overrides / segment prefixes
        self.assertAsm("jmp equality\nequality: call tss_handler\ntss_handler: ret\n", "eb 00 e8 00 00 c3", nasm=True)
        self.assertAsm("mov al, [bytecount]\nmov ax, [wordy]\nbytecount: db 5\nwordy: dw 1\n",
                       "a0 06 00 a1 07 00 05 01 00", nasm=True)
        self.assertAsm("lines: nop\nnames: jmp lines\n", "90 eb fd", nasm=True)

    def test_label_on_same_line_as_data(self):
        self.assertAsm("msg: db 'hi', 0\nptr: dw msg\n", "68 69 00 00 00", nasm=True)

    def test_equ(self):
        self.assertAsm("five equ 5\nten equ five*2\nmov ax, ten\nfwd equ later+1\nmov bx, fwd\nlater:\n",
                       "b8 0a 00 bb 07 00", nasm=True)

    def test_equ_chain_forward(self):
        self.assertAsm("a equ b+1\nb equ c*2\nc equ 5\ndb a, b, c\n", "0b 0a 05", nasm=True)

    def test_predefined_descriptor_constants(self):
        self.assertAsm("dw D_CODE, D_DATA, D_PRESENT, D_BIG_LIM\n", "00 18 00 10 00 80 80 00")

    def test_predefined_constant_can_be_redefined(self):
        self.assertAsm("D_CODE equ 7\ndb D_CODE\n", "07")

    def test_many_labels(self):
        src = "".join(f"label_{i}: dw label_{i}\n" for i in range(3000))
        out = self.assertAsm(src, "".join(f"{(2 * i) & 0xff:02x} {(2 * i) >> 8:02x}" for i in range(3000)))
        self.assertEqual(len(out), 6000)


class TestBranches(AsmTestCase):
    def test_short_forward_boundary(self):
        # +127 is the furthest short jump; +128 needs a near jump
        src = "jmp t1\ntimes 127 nop\nt1: jmp t2\ntimes 128 nop\nt2: nop\n"
        self.assertAsm(src, "eb 7f" + "90" * 127 + "e9 80 00" + "90" * 128 + "90", nasm=True)

    def test_short_backward_boundary(self):
        src = "t0: times 126 nop\njmp t0\nt1: times 127 nop\njmp t1\n"
        self.assertAsm(src, "90" * 126 + "eb 80" + "90" * 127 + "e9 7e ff", nasm=True)

    def test_jump_chain_converges(self):
        src = "".join(f"jmp L{i}\n" for i in range(40)) + "times 60 nop\n" + "".join(f"L{i}: nop\n" for i in range(40))
        out = self.assertAsm(src, nasm_assemble(src).hex() if NASM else
                             "e99400e99200e99000e98e00e98c00e98a00e98800e98600e98400e98200e98000eb7feb7eeb7deb7ceb7beb7a"
                             "eb79eb78eb77eb76eb75eb74eb73eb72eb71eb70eb6feb6eeb6deb6ceb6beb6aeb69eb68eb67eb66eb65eb64eb63"
                             + "90" * 100)
        self.assertEqual(len(out), 11 * 3 + 29 * 2 + 100)

    def test_far_backward_jump_to_low_address(self):
        # A backward jump to a label near address 0 must not use a truncated short jump
        out = self.assertAsm("x: nop\ntimes 0x300 nop\njmp x\n", "90" * 0x301 + "e9 fc fc", nasm=True)
        self.assertEqual(len(out), 0x304)

    def test_conditional_jumps_16(self):
        self.assertAsm("l: jz l2\ntimes 200 nop\nl2: jnz l\njc l2\n",
                       "0f 84 c8 00" + "90" * 200 + "0f 85 30 ff 72 fa", nasm=True)

    def test_conditional_jumps_32(self):
        self.assertAsm("bits 32\nl: jz l2\ntimes 200 nop\nl2: jnz l\ncall l\n",
                       "0f 84 c8 00 00 00" + "90" * 200 + "0f 85 2c ff ff ff e8 27 ff ff ff", nasm=True)

    def test_short_and_near_keywords(self):
        self.assertAsm("jmp short x\njmp near x\nx: jz near x\n", "eb 03 e9 00 00 0f 84 fc ff", nasm=True)

    def test_loop_and_jcxz(self):
        self.assertAsm("l: loop l\nloopz l\nloopnz l\njcxz l\njecxz l\nbits 32\njecxz l\njcxz l\n",
                       "e2 fe e1 fc e0 fa e3 f8 67 e3 f5 e3 f3 67 e3 f0", nasm=True)

    def test_call_and_ret(self):
        self.assertAsm("call f\nret\nf: ret 4\nretf\nretf 8\n", "e8 01 00 c3 c2 04 00 cb ca 08 00", nasm=True)

    def test_far_jumps(self):
        self.assertAsm("jmp 0x1234:0x5678\ncall 0x10:label\njmp dword 0x08:0x12345678\njmp far [bx]\ncall far [di+4]\nlabel:\n",
                       "ea 78 56 34 12 9a 17 00 10 00 66 ea 78 56 34 12 08 00 ff 2f ff 5d 04", nasm=True)

    def test_jmp_dollar(self):
        self.assertAsm("jmp $\nbits 32\njmp $\n", "eb fe eb fe", nasm=True)


class TestDirectives(AsmTestCase):
    def test_strings(self):
        self.assertAsm("db 'abc', \"d'e\", `f`, 0\ndw 'a', 'abc'\ndd 'abcde'\ndb 'semi;colon' ; real comment\n",
                       "61 62 63 64 27 65 66 00 61 00 61 62 63 00 61 62 63 64 65 00 00 00"
                       "73 65 6d 69 3b 63 6f 6c 6f 6e", nasm=True)

    def test_string_followed_by_expression(self):
        self.assertAsm("db 'a'+1, 'b'\n", "62 62", nasm=True)

    def test_dw_dd_values(self):
        self.assertAsm("dw 0x1234, -1, label\ndd 0x12345678, label\nlabel:\n", "34 12 ff ff 0e 00 78 56 34 12 0e 00 00 00", nasm=True)

    def test_long_data_line(self):
        # The original fixed 500 byte buffer overflowed on long dd lines
        src = "dd " + ", ".join(str(i) for i in range(400)) + "\n"
        out = self.assertAsm(src, "".join(f"{i & 0xff:02x} {i >> 8:02x} 00 00" for i in range(400)), nasm=True)
        self.assertEqual(len(out), 1600)

    def test_times(self):
        self.assertAsm("times 4 db 0xAA\ntimes 2 dw 1, 2\ntimes 3 nop\ntimes 2+1 db 7\ntimes 0 db 1\n",
                       "aa aa aa aa 01 00 02 00 01 00 02 00 90 90 90 07 07 07", nasm=True)

    def test_times_boot_sector_padding(self):
        out = self.assertAsm("org 0x7c00\njmp $\ntimes 510-($-$$) db 0\ndw 0xAA55\n",
                             "eb fe" + "00" * 508 + "55 aa", nasm=True)
        self.assertEqual(len(out), 512)

    def test_reserve(self):
        self.assertAsm("resb 2\nresw 2\nresd 1\ndb 1\n", "00 00 00 00 00 00 00 00 00 00 01", nasm=True)

    def test_align(self):
        self.assertAsm("db 1\nalign 4\ndb 2\nalign 8, db 0xCC\nalignb 4\ndb 3\n",
                       "01 90 90 90 02 cc cc cc 03", nasm=True)

    def test_bits_forms(self):
        self.assertAsm("[BITS 32]\nnop\nmov ax, 1\nbits 16\nmov ax, 1\n[bits 32]\nmov eax, 1\n",
                       "90 66 b8 01 00 b8 01 00 b8 01 00 00 00", nasm=True)

    def test_org_forms(self):
        self.assertAsm("[ORG 0x100]\nx: dw x\n", "00 01", nasm=True)
        self.assertAsm("org 0x7c00\nx: dw x\n", "00 7c", nasm=True)

    def test_include(self):
        res = assemble('db 1\n%include "inc/defs.inc"\ndb VALUE\n',
                       files={"inc/defs.inc": "VALUE equ 0x42\ndb 2\n%include \"more.inc\"\n",
                              "inc/more.inc": "db 3\n"})
        self.assertTrue(res.ok, res.stderr)
        self.assertEqual(res.output.hex(), "01020342")

    def test_desc_segment(self):
        # flat 4GB code and data segments
        self.assertAsm("desc 0, 0xFFFFF, D_CODE + D_READ + D_BIG + D_BIG_LIM\n"
                       "desc 0, 0xFFFFF, D_DATA + D_WRITE + D_BIG + D_BIG_LIM\n"
                       "desc 0x12345678, 0xABCDE, D_DATA + D_DPL3\n",
                       "ff ff 00 00 00 9a cf 00  ff ff 00 00 00 92 cf 00  de bc 78 56 34 f0 0a 12")

    def test_desc_gate(self):
        self.assertAsm("desc 0x12345678, 0x08, D_INT + D_DPL3\ndesc handler, 0x10, D_TRAP\nhandler:\n",
                       "78 56 08 00 00 ee 34 12  10 00 10 00 00 8f 00 00")

    def test_desc_not_present(self):
        self.assertAsm("desc 0, 0xFFFF, D_DATA + D_NOT_PRESENT\n", "ff ff 00 00 00 10 00 00")

    def test_tss(self):
        out = self.assertAsm("nop\nmytss tss\ndw mytss, mytss.esp0, mytss.cr3, mytss.io\nmov eax, [mytss.ss0]\n",
                             "90" + "00" * 104 + "01 00 05 00 1d 00 67 00 66 a1 09 00")
        self.assertEqual(len(out), 1 + 104 + 8 + 4)

    def test_tss_does_not_change_local_label_scope(self):
        # the TSS label opens a scope; its generated field labels must not replace it
        self.assertAsm("t tss\ndw .esp0, .io, t.ss0\n", "00" * 104 + "04 00 66 00 08 00")

    def test_mnemonic_followed_by_directive_name(self):
        # "equ"/"tss" after an instruction are operands, not directives
        res = assemble("push tss\n")
        self.assertEqual(res.returncode, 1)
        self.assertIn("undefined label/identifier 'tss'", res.stderr)

    def test_comments_and_whitespace(self):
        self.assertAsm("; full line comment\n\n\t  mov ax , bx ; trailing\r\n\tret\t\n   \n", "89 d8 c3", nasm=True)

    def test_empty_source(self):
        res = assemble("")
        self.assertTrue(res.ok, res.stderr)
        self.assertEqual(res.output, b"")

    def test_no_trailing_newline(self):
        self.assertAsm("nop\nret", "90 c3", nasm=True)


class TestEncodings(AsmTestCase):
    def test_original_readme_sample(self):
        self.assertAsm("mov ax, bx\nmov al, 5\nmov ax, 1234h\nmov [bx], ax\nmov ax, [bx+si+4]\nadd ax, 10\nint 21h\nret\n",
                       "89 d8 b0 05 b8 34 12 89 07 8b 40 04 83 c0 0a cd 21 c3", nasm=True)

    def test_memory_16(self):
        self.assertAsm("mov ax, [bx]\nmov ax, [bp]\nmov ax, [bp+2]\nmov ax, [si+bx+0x1000]\nmov ax, [di-1]\n"
                       "mov ax, [bx+di]\nmov ax, [bp+si]\nmov ax, [0x1234]\nmov cx, [0x1234]\n",
                       "8b 07 8b 46 00 8b 46 02 8b 80 00 10 8b 45 ff 8b 01 8b 02 a1 34 12 8b 0e 34 12", nasm=True)

    def test_memory_32(self):
        self.assertAsm("bits 32\nmov eax, [esp]\nmov eax, [ebp]\nmov eax, [ebx*4]\nmov eax, [ecx*2]\n"
                       "mov eax, [esp+eax*8+0x100]\nmov eax, [4*esi+edi-8]\nmov eax, [eax+esp]\n",
                       "8b 04 24 8b 45 00 8b 04 9d 00 00 00 00 8b 04 09 8b 84 c4 00 01 00 00 8b 44 b7 f8 8b 04 04",
                       nasm=True)

    def test_forward_label_displacement_shrinks(self):
        # gasm resolves forward references before choosing displacement sizes, so a small
        # forward label gets a disp8 (NASM keeps disp16/disp32 here)
        out = self.assertAsm("mov ax, [bx+si+label]\nlabel:\n", "8b 40 03")
        if NDISASM:
            self.assertEqual(disassemble(out, 16), ["mov ax,[bx+si+0x3]"])

    def test_address_and_operand_size_overrides(self):
        self.assertAsm("mov ax, [ebx]\nmov eax, [bx]\nmov eax, [ebx]\nbits 32\nmov eax, [bx+si]\nmov ax, [ebx]\n",
                       "67 8b 03 66 8b 07 66 67 8b 03 67 8b 00 66 8b 03", nasm=True)

    def test_segment_overrides(self):
        self.assertAsm("mov ax, [es:bx]\nmov [cs:0x10], al\nmov eax, [fs:esi+4]\nmov al, [gs:di]\nlea ax, [ss:bp]\n",
                       "26 8b 07 2e a2 10 00 64 66 67 8b 46 04 65 8a 05 36 8d 46 00", nasm=True)

    def test_gasm_segment_override_syntax(self):
        # gasm also accepts the override outside the brackets
        self.assertAsm("mov ax, es:[si]\nmov byte fs:[bx], 1\n", "26 8b 04 64 c6 07 01")

    def test_size_keywords(self):
        self.assertAsm("mov byte [bx], 1\nmov word [bx], 1\nmov dword [bx], 1\nadd dword [bx], 5\ninc byte [si]\n"
                       "push word [bx]\npush dword [bx]\n",
                       "c6 07 01 c7 07 01 00 66 c7 07 01 00 00 00 66 83 07 05 fe 04 ff 37 66 ff 37", nasm=True)

    def test_unsized_memory_defaults_to_bits(self):
        # NASM requires a size here; gasm defaults to the current BITS size
        self.assertAsm("inc [bx]\nmov [bx], 5\nbits 32\ninc [ebx]\n", "ff 07 c7 07 05 00 ff 03")

    def test_sign_extended_immediates(self):
        self.assertAsm("add ax, 5\nadd ax, 0x80\nadd ax, 0xFFFF\nadd eax, 0xFFFFFFFF\nadd al, 0x80\nand cx, -128\nor cx, -129\n",
                       "83 c0 05 05 80 00 83 c0 ff 66 83 c0 ff 04 80 83 e1 80 81 c9 7f ff", nasm=True)

    def test_accumulator_forms(self):
        self.assertAsm("add al, 1\nadd ax, 0x1234\nadd eax, 0x12345678\ntest al, 5\ntest ax, 0x100\n",
                       "04 01 05 34 12 66 05 78 56 34 12 a8 05 a9 00 01", nasm=True)

    def test_moffs_forms(self):
        self.assertAsm("mov al, [0x10]\nmov [0x10], ax\nbits 32\nmov eax, [0x1000]\nmov [es:0x1000], al\n",
                       "a0 10 00 a3 10 00 a1 00 10 00 00 26 a2 00 10 00 00", nasm=True)

    def test_segment_register_moves(self):
        self.assertAsm("mov ds, ax\nmov es, [bx]\nmov ax, cs\nmov [bx], ss\nbits 32\nmov ds, ax\nmov ax, ds\n",
                       "8e d8 8e 07 8c c8 8c 17 8e d8 66 8c d8", nasm=True)

    def test_push_pop(self):
        self.assertAsm("push 5\npush 0x1234\npush ax\npush word [bx]\npop dword [ebx]\npush es\npop ds\npush fs\npop gs\npush cs\n",
                       "6a 05 68 34 12 50 ff 37 66 67 8f 03 06 1f 0f a0 0f a9 0e", nasm=True)

    def test_push_imm_32(self):
        self.assertAsm("bits 32\npush 5\npush 0x1234\npush -1\n", "6a 05 68 34 12 00 00 6a ff", nasm=True)

    def test_imul_forms(self):
        self.assertAsm("imul ax, bx, 5\nimul ax, [bx], 300\nimul eax, 10\nimul ecx, edx, -1\nimul cx\nimul ax, bx\n",
                       "6b c3 05 69 07 2c 01 66 6b c0 0a 66 6b ca ff f7 e9 0f af c3", nasm=True)

    def test_shifts(self):
        self.assertAsm("shl ax, 1\nshr byte [bx], 1\nrol eax, 1\nsar cx, 3\nrcl dx, cl\nsal bl, 2\n",
                       "d1 e0 d0 2f 66 d1 c0 c1 f9 03 d3 d2 c0 e3 02", nasm=True)

    def test_in_out(self):
        self.assertAsm("in al, 0x60\nin ax, dx\nin eax, dx\nout dx, al\nout 0x80, eax\n",
                       "e4 60 ed 66 ed ee 66 e7 80", nasm=True)

    def test_test_is_commutative(self):
        self.assertAsm("test al, bl\ntest [bx], al\ntest al, [bx]\ntest ax, 0x100\n", "84 d8 84 07 84 07 a9 00 01", nasm=True)

    def test_xchg(self):
        self.assertAsm("xchg ax, bx\nxchg bx, ax\nxchg al, bl\nxchg [bx], cx\nxchg cx, [bx]\nxchg eax, ecx\n",
                       "93 93 86 d8 87 0f 87 0f 66 91", nasm=True)

    def test_movzx_movsx(self):
        self.assertAsm("movzx ax, bl\nmovzx eax, byte [si]\nmovzx eax, word [si]\nmovsx eax, word [si]\nmovsx cx, byte [bx]\n",
                       "0f b6 c3 66 0f b6 04 66 0f b7 04 66 0f bf 04 0f be 0f", nasm=True)

    def test_system_instructions(self):
        self.assertAsm("bits 32\nlgdt [gdtr]\nlidt [eax]\nmov eax, cr0\nmov cr0, eax\nmov dr7, eax\nltr ax\nlldt [ebx]\nstr ax\n"
                       "sldt [ebx]\ngdtr:\n",
                       "0f 01 15 20 00 00 00 0f 01 18 0f 20 c0 0f 22 c0 0f 23 f8 0f 00 d8 0f 00 13 66 0f 00 c8 0f 00 03",
                       nasm=True)

    def test_prefixes(self):
        self.assertAsm("rep movsb\nrepne scasb\nrepz cmpsw\nlock xchg [bx], ax\nlock add [bx], ax\nrep\nlock\n",
                       "f3 a4 f2 ae f3 a7 f0 87 07 f0 01 07 f3 f0", nasm=True)

    def test_fixed_instruction_sizes(self):
        # size-neutral instructions get no prefix in either mode; sized ones get 0x66 only
        self.assertAsm("nop\ncwde\ncbw\nmovsd\nmovsw\npusha\npushad\niret\niretd\nfwait\n"
                       "bits 32\nnop\ncwde\ncbw\nmovsd\nmovsw\npusha\npushad\niret\niretd\nfwait\n",
                       "90 66 98 98 66 a5 a5 60 66 60 cf 66 cf 9b  90 98 66 98 a5 66 a5 60 60 cf cf 9b", nasm=True)

    def test_three_byte_fixed_instructions(self):
        self.assertAsm("finit\nfclex\nfninit\n", "9b db e3 9b db e2 db e3", nasm=True)

    def test_misc_fixed(self):
        self.assertAsm("int3\nxlatb\nxlat\ncpuid\nrdtsc\nhlt\n", "cc d7 d7 0f a2 0f 31 f4", nasm=True)


class TestRegressions(AsmTestCase):
    """Bugs in the original gasm 0.55, each reproduced against the original binary."""

    def test_h_suffix_hex(self):
        # was: "int 21h" -> int 0x15 (decimal 21); "mov bx, 0FFh" -> mov bx, 0
        self.assertAsm("int 21h\nmov bx, 0FFh\nmov ax, 1234h\n", "cd 21 bb ff 00 b8 34 12", nasm=True)

    def test_byte_register_with_memory(self):
        # was: "You have entered arguments not possible for the instruction"
        self.assertAsm("mov al, [bx]\nadd [si], cl\ncmp bh, [di]\n", "8a 07 00 0c 3a 3d", nasm=True)

    def test_no_spurious_address_prefix_on_register_operands(self):
        # was: "add eax, eax" -> 66 67 01 c0
        self.assertAsm("add eax, eax\nmov ecx, edx\n", "66 01 c0 66 89 d1", nasm=True)

    def test_no_prefixes_on_neutral_instructions_in_32bit(self):
        # was: nop in BITS 32 -> 66 67 90
        self.assertAsm("bits 32\nnop\nhlt\nret\n", "90 f4 c3", nasm=True)

    def test_bp_without_displacement(self):
        # was: "Invalid arguments in memory operand"
        self.assertAsm("mov ax, [bp]\nmov ax, [bp+di]\nbits 32\nmov eax, [ebp]\nmov eax, [esp]\n",
                       "8b 46 00 8b 03 8b 45 00 8b 04 24", nasm=True)

    def test_expression_precedence(self):
        # was: 2+3*4 = 20 and -1+2 = -3
        self.assertAsm("db 2+3*4, -1+2\n", "0e 01", nasm=True)

    def test_cmp_reg8_mem_table_entry(self):
        # table entry declared "cmp r8, r/m16"; in 32-bit mode this produced an o16 prefix
        self.assertAsm("bits 32\ncmp al, [ebx]\n", "3a 03", nasm=True)

    def test_movsx_word_opcode(self):
        # was: movsx eax, word [bx] -> 0f be (movsx from byte)
        self.assertAsm("movsx eax, word [bx]\n", "66 0f bf 07", nasm=True)

    def test_dword_memory_operand_size(self):
        # was: "add dword [bx], 5" -> 83 07 05 (a word operation)
        self.assertAsm("add dword [bx], 5\n", "66 83 07 05", nasm=True)

    def test_three_operand_imul_not_dropped(self):
        # was: "imul ax, bx, 5" -> 0f af c3 (the 5 was silently ignored)
        self.assertAsm("imul ax, bx, 5\n", "6b c3 05", nasm=True)

    def test_semicolon_in_string(self):
        # was: the line was cut at the ';' -> 'a' only
        self.assertAsm("db 'a;b'\n", "61 3b 62", nasm=True)

    def test_label_starting_with_tss_or_equ(self):
        # was: "call tss_handler" -> "Internal error.. Handletss()"
        self.assertAsm("call tss_handler\ntss_handler: ret\njmp equals\nequals: ret\n", "e8 00 00 c3 eb 00 c3", nasm=True)

    def test_label_containing_segment_name(self):
        # was: any line containing "es:" (e.g. a label "lines:") got a 0x26 prefix
        self.assertAsm("lines: nop\n", "90", nasm=True)

    def test_fixed_32bit_string_op_has_no_address_prefix(self):
        # was: movsd in BITS 16 -> 66 67 a5 (0x67 switched to esi/edi addressing)
        self.assertAsm("movsd\nstosd\n", "66 a5 66 ab", nasm=True)

    def test_short_jump_target_not_truncated(self):
        # relative targets were computed from an 8-bit truncated target address
        self.assertAsm("org 0x100\nl: jmp short l2\nl2: jmp short l\n", "eb 00 eb fc", nasm=True)

    def test_32bit_near_branches(self):
        # BITS 32 branches must use rel32, not o16 rel16
        out = self.assertAsm("bits 32\njmp x\ntimes 200 nop\nx:\n", "e9 c8 00 00 00" + "90" * 200, nasm=True)
        self.assertEqual(len(out), 205)

    def test_three_byte_opcode_copy(self):
        # a 3-byte dynamic opcode used to skip writing the second byte
        self.assertAsm("fclex\n", "9b db e2", nasm=True)


if __name__ == "__main__":
    unittest.main()
