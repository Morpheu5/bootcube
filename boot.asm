bits 16     ; We're dealing with 16 bit code
org 0x7C00  ; Inform the assembler of the starting location for this code
            ; This is just to generate the right addresses.

stackSize   equ 0x40                ; 0x40 (1024/16 stack size)
stackBase   equ 0x7E0 + stackSize   ; 0x7E0 * 16 is the first available byte after the code


main:
    ; Make sure the stack is ready, just in case we need it...
    push stackBase
    pop ss
    mov sp, stackSize
    mov bp, sp

    ; Change video mode to MCGA 320x300, 256 indexed colors
    mov ax, 0x13
    int 10h

    ; Draw a pixel on screen because we can - this will likely go away to save space...
    mov ax, 0xA000
    mov es, ax
    mov bx, 10
    mov al, 7
    mov es:[bx], al

    ; Run through all the colors several time and clear the screen each time
    mov cx, 4096*4
.loopsydo:
    dec cx
    jz .loop
    push cx
    call clearScreen
    mov edx, 0x1000000
.wasteTime:
    dec edx
    jnz .wasteTime
    jmp .loopsydo

    ; ... and curtains.
.loop:
    jmp .loop
    hlt

    ; Needs the color on the stack
clearScreen:
    push bp
    mov bp, sp
    ; bx is going to be the counter here
    mov bx, 0xFA00
    mov ax, [bp + 4]
clearScreen_loop:
    mov es:bx, al
    dec bx
    jnz clearScreen_loop
    xor bx, bx
    mov es:bx, al
    pop bp
    ret



times 510-($-$$) db 0 ; Add enough padding to make 510 bytes in total
db 0x55, 0xAA ; Boot magic number 0xAA55
