_main:
    subq $24, %rsp
    leaq _mode(%rip), %r11
    mov %r11, 0(%rsp)
    leaq _mode(%rip), %r11
    mov %r11, 16(%rsp)
    mov 16(%rsp), %rsi
    leaq 8(%rsp), %rdi
    movsq
    leaq 8(%rsp), %rax
    addq $24, %rsp
    ret
    addq $24, %rsp
    ret
