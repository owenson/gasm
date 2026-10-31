; Boot sector: prints a message using the BIOS, then halts.
; NASM compatible; tests/programs/boot.bin was produced by NASM.
[BITS 16]
[ORG 0x7C00]

start:
	cli
	xor ax, ax
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, 0x7C00
	sti
	mov [boot_drive], dl

	mov si, msg_hello
	call print
	mov si, msg_info
	call print

	; count down from 5, printing digits
	mov cx, 5
.count:
	mov al, '0'
	add al, cl
	call putc
	loop .count

	call newline
	jmp halt

; print zero terminated string at ds:si
print:
	pusha
.next:
	lodsb
	or al, al
	jz .done
	call putc
	jmp short .next
.done:
	popa
	ret

putc:
	push bx
	mov ah, 0x0E
	mov bx, 0x0007
	int 0x10
	pop bx
	ret

newline:
	mov al, 13
	call putc
	mov al, 10
	jmp putc

halt:
	hlt
	jmp halt

boot_drive:	db 0
msg_hello:	db "Hello from gasm; ", 0
msg_info:	db 'Size=', (end_code - start) / 10 + '0', (end_code - start) % 10 + '0', 13, 10, 0
end_code:

	times 510 - ($ - $$) db 0
	dw 0xAA55
