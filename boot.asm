%include "macros.inc"

bits 16     ; We're dealing with 16 bit code
org 0x7C00  ; Inform the assembler of the starting location for this code
            ; This is just to generate the right addresses.
    jmp 0:main ; And this is to make sure CS is in the right place.

stackSize   equ 0x40                ; 0x40 (1024/16 stack size)
stackBase   equ 0x800 + stackSize   ; 0x800 * 16 is a good location for the stack

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

    push 100 ; Make sure these are the right size
    push 160
    call coords ; Returns into bx
    add sp, 4
    ; Color the pixel on screen
    mov al, 15 ; Sets color to white
    drawPixel ; Draws a white pixel to the given ccordinates

; ... and curtains.
exit:
    jmp exit
    hlt

matMul:
        push    ebp
        mov     ebp, esp
        xor     ecx, ecx
        push    edi
        push    esi
        xor     esi, esi
        push    ebx
        sub     esp, 16
        movsx   ebx, byte [ebp+20]
        mov     dword [ebp-20], ecx
        lea     eax, [0+ebx*4]
        mov     dword [ebp-28], eax
    .L2:
        mov     edx, dword [ebp-20]
        mov     eax, dword [ebp+16]
        mov     edi, dword [ebp+8]
        lea     eax, [eax+edx*4]
        lea     edx, [edi+esi*4]
        mov     dword [ebp-24], edx
        xor     edx, edx
        mov     dword [ebp-16], edx
    .L6:
        cmp     dword [ebp-16], ebx
        jnb     .L10
        mov     edi, dword [ebp-16]
        mov     ecx, dword [ebp+12]
        mov     dword [eax], 0x00000000
        lea     ecx, [ecx+edi*4]
        mov     edi, ecx
        xor     ecx, ecx
    .L3:
        mov     edx, dword [ebp-24]
        fld     dword [edx+ecx*4]
        fmul    dword [edi]
        mov     edx, edi
        inc     ecx
        fadd    dword [eax]
        mov     edi, dword [ebp-28]
        add     edx, edi
        mov     edi, edx
        fstp    dword [eax]
        cmp     ecx, 4
        jne     .L3
        inc     dword [ebp-16]
        add     eax, 4
        jmp     .L6
    .L10:
        add     esi, 4
        add     dword [ebp-20], ebx
        cmp     esi, 16
        jne     .L2
        add     esp, 16
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
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
    dd  0.0  ,  0.0  ,  0.0  ,  1.0
    dd  1.0  ,  0.0  ,  0.0  ,  1.0

lineToDraw:
    times 4 dd 0

times 1024-($-$$) db 0 ; Add enough padding to make 1024 bytes in total
