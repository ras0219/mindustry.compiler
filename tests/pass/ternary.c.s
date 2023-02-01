_f:
    subq $24, %rsp
    mov %edi, 0(%rsp)
    movsl 0(%rsp), %rax
    cmp $1, %eax
    sete %al
    movzx %al, %rax
    mov %rax, 8(%rsp)
    movsl 8(%rsp), %rax
    cmp $0, %rax
    jz  L$1
    movl $10, 4(%rsp)
    jmp L$2
L$1:
    movl $20, 4(%rsp)
L$2:
    movsl 4(%rsp), %rax
    addq $24, %rsp
    ret
    addq $24, %rsp
    ret
