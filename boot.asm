%include "macros.inc"

bits 16     ; We're dealing with 16 bit code
org 0x7C00  ; Inform the assembler of the starting location for this code
            ; This is just to generate the right addresses.
    jmp 0:main ; And this is to make sure CS is in the right place.

stackSize   equ 0x1000
stackBase   equ 0x900    ; 0x9000 is a good location for the stack

main:
    ; Make sure the stack is ready, just in case we need it...
    push stackBase
    pop ss
    mov sp, stackSize
    mov bp, sp

    ; Load an extra sector (dl contains the drive number)
    xor ah, ah  ; Reset floppy controller
    int 0x13

    mov ax, 0x201 ; Load sector 2 from the boot drive
    mov cx, 0x2   ; Somehow, the sectors start counting from 1, so the second one is number 2...
    xor dh, dh
    xor bx, bx
    mov es, bx
    mov bx, 0x7e00
    int 0x13

    ; Change video mode to MCGA 320x300, 256 indexed colors
    mov ax, 0x13
    int 10h

    ; Prepare the video segment
    mov ax, 0xA000
    mov es, ax

    push 54
    call clearScreen
    add sp, 2

    finit

    push dword 4
    lea eax, [lineToDraw]
    push eax
    lea eax, [line]
    push eax
    lea eax, [rotMat]
    push eax

    call matMul
    add sp, 16
    debug
    ; push 100 ; Make sure these are the right size
    ; push 160
    ; call coords ; Returns into bx
    ; add sp, 4
    ; ; Color the pixel on screen
    ; mov al, 15 ; Sets color to white
    ; drawPixel ; Draws a white pixel to the given ccordinates

; ... and curtains.
exit:
    jmp exit
    hlt

matMul:
        push bp
        mov bp, sp
        sub sp, 12

        xor esi, esi
        mov word [bp - 2], 0 ; i = 0
    .L1:
        cmp word [bp - 2], 4 ; i < 4
        je .L1e

        mov word [bp - 4], 0 ; j = 0
    .L2:
        mov ax, [bp + 16] ; ax = stride
        cmp [bp - 4], ax ; j < stride
        je .L2e

        ; Calculate Cidx
        mul word [bp - 2] ; stride * i -- This may result in overflows if the stride is too large
        add ax, word [bp - 4] ; stride * i + j
        shl ax, 2
        mov [bp - 8], ax  ; Cidx = stride * i + j
        ; Set C[Cidx] = 0.0
        mov bx, ax ; ax can't be used in effective address calculations, apparently
        add bx, [bp + 12]
        mov dword [bx], 0
    
        mov word [bp - 6], 0 ; k = 0
    .L3:
        cmp word [bp - 6], 4 ; k < 4
        je .L3e

        ; Calculate Bidx
        mov ax, [bp + 16]
        mul word [bp - 6]
        add ax, word [bp - 4]
        shl ax, 2
        mov [bp - 10], ax
        ; Calculate Aidx
        mov ax, [bp - 2]
        shl ax, 2
        add ax, [bp - 4]
        shl ax, 2
        mov [bp - 12], ax
        ; Time for the hard maths
        mov si, [bp - 8]
        add si, [bp + 12]
        fld dword [si]
        mov si, [bp - 10]
        add si, [bp + 8]
        fld dword [si]
        mov si, [bp - 12]
        add si, [bp + 4]
        fld dword [si]
        fmul
        fadd
        mov si, [bp - 8]
        add si, [bp + 12]
        fstp dword [si]

        inc word [bp - 6] ; ++k
        jmp .L3

    .L3e:
        inc word [bp - 4] ; ++j
        jmp .L2

    .L2e:
        inc word [bp - 2] ; ++i
        jmp .L1

    .L1e:
        ; debug
        leave
        ret

plotLine:
    push bp
    mov bp, sp
    ; [bp -  2]  --  dx = abs(x1 - x0)
    mov ax, [bp + 8]  ; x1
    sub ax, [bp + 4]  ; x1 - x0
    ; absw ax           ; abs(x1 - x0)
    call absw
    push ax
    ; [bp -  4]  --  dy = -abs(y1 - y0)
    mov ax, [bp + 10] ; y1
    sub ax, [bp + 6]  ; y1 - y0
    ; absw ax           ; abs(y1 - y0)
    call absw
    neg ax            ; -abs(y1 - y0)  --  OK, this MAY be optimizable away with a smarter abs routine
    push ax
    ; [bp -  6]  --  sx = x0 < x1 ? 1 : -1
    mov di, [bp + 4]
    mov si, [bp + 8]
    call signCmp
    push ax
    ; [bp -  8]  --  sy = y0 < y1 ? 1 : -1
    mov di, [bp + 6]
    mov si, [bp + 10]
    call signCmp
    push ax
    ; [bp - 10]  --  error = dx + dy
    mov ax, [bp - 2]
    add ax, [bp - 4]
    push ax

    .loop: ; while (1) {
        ; plot(x0, y0)
        push word [bp + 6]
        push word [bp + 4]
        call coords
        add sp, 4
        mov al, 15 ; Set a nice red color
        drawPixel
        ; if (x0 == x1 && y0 == y1) break;
        mov ax, [bp + 4]
        cmp [bp + 8], ax
        jne .ahead ; x0 != x1
        mov ax, [bp + 6]
        cmp [bp + 10], ax
        je .return ; y0 == y1 && x0 == x1

    .ahead:
        mov di, [bp - 10]
        shl di, 1 ; e2 = 2 * error
        ; if (e2 >= dy) {
        cmp di, [bp - 4]
        jl .fidy
            ; if x0 == x1 break
            mov ax, [bp + 4]
            cmp [bp + 8], ax
            je .return
            ; error = error + dy
            mov dx, [bp - 4]
            add [bp - 10], dx
            ; x0 = x0 + sx
            mov dx, [bp - 6]
            add [bp + 4], dx
        ; }
    .fidy:
        ; if (e2 <= dx) {
        cmp di, [bp - 2]
        jg .fidx
            ; if y0 == y1 break
            mov ax, [bp + 6]
            cmp [bp + 10], ax
            je .return
            ; error = error + dx
            mov dx, [bp - 2]
            add [bp - 10], dx
            ; y0 = y0 + sy
            mov dx, [bp - 8]
            add [bp + 6], dx
        ; }
    .fidx:
        
        jmp plotLine.loop
    ; }
    .return:
    leave
    ret

absw: ; WARNING This changes bx
    mov bx, ax
    neg ax
    cmovl ax, bx ; if bx is negative, restore it, otherwise it was positive
    ret

signCmp: ; signCmp(di, si)
    xor ax, ax
    cmp di, si   ; sub di, si  ->  di < si ?  ->  a < b ?
    setl al      ; a < b ? al = 1 : al = 0
    xchg bx, bx
    add ax, ax   ; ax + ax  ->  ax == 0 ? 0 : 2
    dec ax       ; ax--  ->  ax == 0 ? -1 : 1
    ret

coords: ; Returning into bx goes against convention but it makes for more compact code
    push bp
    mov bp, sp
    imul bx, [bp + 6], 320 ; Well, this is hard-coded. Tough luck.
    add bx, [bp + 4]
    leave
    ret

; Needs the color on the stack
clearScreen:
    push bp
    mov bp, sp
    ; bx is going to be the counter here
    mov bx, 0xFA00
    mov ax, [bp + 4]
    .loop:
        mov es:bx, al
        dec bx
        jnz .loop
        xor bx, bx
        mov es:bx, al
        pop bp
        ret




times 510-($-$$) db 0 ; Add enough padding to make 510 bytes in total
db 0x55, 0xAA ; Boot magic number 0xAA55

rotMat:
    dd  0.5  , -0.707,  0.0  ,  0.0
    dd  0.707,  0.5  ,  0.0  ,  0.0
    dd  0.0  ,  0.0  ,  1.0  ,  0.0
    dd  0.0  ,  0.0  ,  0.0  ,  1.0

line:
    dd  1.0  ,  0.0  ,  0.0  ,  0.0
    dd  0.0  ,  1.0  ,  0.0  ,  0.0
    dd  0.0  ,  0.0  ,  1.0  ,  0.0
    dd  1.0  ,  1.0  ,  1.0  ,  1.0

lineToDraw:
    times 16 dd 0x11111111

times 1024-($-$$) db 0 ; Add enough padding to make 1024 bytes in total
