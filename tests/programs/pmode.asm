; Switches to 32-bit protected mode. Uses gasm's own syntax:
; colonless labels, desc/tss directives, D_* constants and seg:[mem] overrides.
; tests/programs/pmode.bin was checked against an equivalent NASM source.
[BITS 16]
[ORG 0x7C00]

CODE_SEL	equ	gdt_code - gdt
DATA_SEL	equ	gdt_data - gdt
TSS_SEL		equ	gdt_tss - gdt

entry	cli
	xor ax, ax
	mov ds, ax
	mov word es:[0x0400], 0
	lgdt [gdtr]
	lidt [idtr]
	mov eax, cr0
	or al, 1
	mov cr0, eax
	jmp dword CODE_SEL:pm_start

[BITS 32]
pm_start
	mov ax, DATA_SEL
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov esp, 0x90000
	mov dword [task.esp0], esp
	mov word [task.ss0], DATA_SEL
	mov ax, TSS_SEL
	ltr ax
	mov edi, 0xB8000
	mov esi, message
	mov ah, 0x1F
.copy	lodsb
	test al, al
	jz .done
	stosw
	jmp .copy
.done	int 0x20
	hlt
	jmp $

handler	pushad
	mov byte [0xB8000 + 2*80], '!'
	popad
	iretd

message	db 'Protected mode', 0

	align 8
gdt	dd 0, 0
gdt_code	desc 0, 0xFFFFF, D_CODE + D_READ + D_BIG + D_BIG_LIM
gdt_data	desc 0, 0xFFFFF, D_DATA + D_WRITE + D_BIG + D_BIG_LIM
gdt_tss	desc task, 103, D_TSS
gdt_end

idt	desc handler, CODE_SEL, D_INT
	desc handler, CODE_SEL, D_TRAP + D_DPL3
idt_end

gdtr	dw gdt_end - gdt - 1
	dd gdt
idtr	dw idt_end - idt - 1
	dd idt

task	tss
