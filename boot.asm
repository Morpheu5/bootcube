%include "macros.inc"

bits 16     ; We're dealing with 16 bit code
org 0x7C00  ; Inform the assembler of the starting location for this code
            ; This is just to generate the right addresses.
    jmp 0:main ; And this is to make sure CS is in the right place.

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

    ; Prepare the video segment
    mov ax, 0xA000
    mov es, ax
    ; Calculate the offset into the video memory from the coordinates
    push 100
    push 160
    call coords
    add sp, 8 ; This is to empty the stack from the two parameters
    ; Write a white pixel to the given ccordinates
    mov bx, ax
    mov al, 7
    mov es:[bx], al

; ... and curtains.
.loop:
    jmp .loop
    hlt

coords: ; This doesn't mess with the stack to reduce code size
        ; NOTE: We COULD go straight for bx instead of ax and save a mov on ret
        ;       but this would sort-of break convention and we'll see if this
        ;       is a problem later.
    mov ax, [bp-2]
    imul ax, 320 ; Well, this is hard-coded. Tough luck.
    add ax, [bp-4]
    ret

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
