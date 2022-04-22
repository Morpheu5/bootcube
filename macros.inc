%macro absq 1 ; WARNING This changes ebx
    mov ebx, %1
    neg %1
    cmovl %1, ebx ; if ebx is negative, restore it, otherwise it was positive
%endmacro

; %macro absw 1 ; WARNING This changes bx
;     mov bx, %1
;     neg %1
;     cmovl %1, bx ; if bx is negative, restore it, otherwise it was positive
; %endmacro

%define drawPixel   mov es:[bx], al