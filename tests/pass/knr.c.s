_compress2:
    subq $56, %rsp
    mov %rdi, 0(%rsp)
    mov %rsi, 8(%rsp)
    mov %rdx, 16(%rsp)
    mov %rcx, 24(%rsp)
    mov %r8d, 32(%rsp)
    movsl 32(%rsp), %rax
    addq $56, %rsp
    ret
    addq $56, %rsp
    ret
_compress3:
    subq $24, %rsp
    mov %rdi, 0(%rsp)
    mov %rsi, 8(%rsp)
    mov 0(%rsp), %rax
    addq $24, %rsp
    ret
    addq $24, %rsp
    ret
_compress4:
    subq $24, %rsp
    mov %edi, 0(%rsp)
    mov %esi, 4(%rsp)
    movsl 4(%rsp), %r11
    movsl 0(%rsp), %r10
    add %r10, %r11
    mov %r11, 8(%rsp)
    movsl 8(%rsp), %rax
    addq $24, %rsp
    ret
    addq $24, %rsp
    ret
