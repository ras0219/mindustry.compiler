_f:
    subq $24, %rsp
    mov %rdi, 0(%rsp)
    mov %esi, 8(%rsp)
    mov %rdx, 16(%rsp)
    leaq 32(%rsp), %rsi
    mov 0(%rsp), %rdi
    mov $32, %rcx
    cld
    rep movsb
    mov 0(%rsp), %rax
    addq $24, %rsp
    ret
    addq $24, %rsp
    ret
