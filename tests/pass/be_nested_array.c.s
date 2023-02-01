_main:
    subq $24, %rsp
    leaq _mode(%rip), %r11
    mov %r11, 8(%rsp)
    mov 8(%rsp), %rsi
    leaq 0(%rsp), %rdi
    movsq
    mov 0(%rsp), %rax
    addq $24, %rsp
    ret
    addq $24, %rsp
    ret
