_f:
    subq $56, %rsp
    mov %edi, 0(%rsp)
    mov %esi, 4(%rsp)
    movsl 0(%rsp), %r11
    movsl 4(%rsp), %rdx
    andq %rdx, %r11
    mov %r11, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 0(%rsp), %r11
    movsl 4(%rsp), %rdx
    orq %rdx, %r11
    mov %r11, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 12(%rsp)
    movsl 0(%rsp), %r11
    movsl 4(%rsp), %rdx
    xorq %rdx, %r11
    mov %r11, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 16(%rsp)
    movsl 0(%rsp), %rax
    not %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 20(%rsp)
    addq $56, %rsp
    ret
