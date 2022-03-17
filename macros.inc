%macro absq 1 ; WARNING This changes ebx
    mov ebx, %1
    neg %1
    cmovl %1, ebx ; if eax is negative, restore it, otherwise it was positive
%endmacro

%macro absw 1 ; WARNING This changes bx
    mov bx, %1
    neg %1
    cmovl %1, bx ; if ax is negative, restore it, otherwise it was positive
%endmacro

%macro abs 1 ; WARNING This changes bl
    mov bl, %1
    neg %1
    cmovl %1, bl ; if al is negative, restore it, otherwise it was positive
%endmacro