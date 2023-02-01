_f:
    subq $24, %rsp
    mov %edi, 0(%rsp)
    jmp L$2
L$3:
    mov $10, %rax
    addq $24, %rsp
    ret
L$4:
    mov $20, %rax
    addq $24, %rsp
    ret
L$5:
    mov $30, %rax
    addq $24, %rsp
    ret
    jmp L$1
L$2:
    movsl 0(%rsp), %rcx
    cmp $1, %rcx
    jz  L$3
    cmp $2, %rcx
    jz  L$4
    jmp L$5
L$1:
    addq $24, %rsp
    ret
