_main:
    subq $24, %rsp
    mov %edi, 0(%rsp)
    mov %rsi, 8(%rsp)
    movsl 0(%rsp), %rax
    addq $24, %rsp
    ret
    addq $24, %rsp
    ret
