_main:
    subq $24, %rsp
    leaq _mode(%rip), %r11
    mov %r11, 0(%rsp)
    leaq _mode(%rip), %r11
    add $5, %r11
    mov %r11, 8(%rsp)
    mov 8(%rsp), %rax
    addq $24, %rsp
    ret
    addq $24, %rsp
    ret
