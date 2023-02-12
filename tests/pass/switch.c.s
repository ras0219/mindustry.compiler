_f:
    subq $24, %rsp
    mov %edi, 0(%rsp)
    jmp L$1
L$2:
    mov $10, %rax
    addq $24, %rsp
    ret
L$3:
    mov $20, %rax
    addq $24, %rsp
    ret
L$4:
    mov $30, %rax
    addq $24, %rsp
    ret
    jmp L$0
L$1:
    movsl 0(%rsp), %rcx
    cmp $1, %rcx
    jz  L$2
    cmp $2, %rcx
    jz  L$3
    jmp L$4
L$0:
    addq $24, %rsp
    ret
