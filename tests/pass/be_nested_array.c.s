_main:
    subq $24, %rsp
    leaq _mode(%rip), %r11
    mov %r11, 0(%rsp)
    leaq _mode+5(%rip), %rax
    addq $24, %rsp
    ret
    addq $24, %rsp
    ret
