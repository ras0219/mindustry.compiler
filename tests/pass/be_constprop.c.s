_main:
    subq $24, %rsp
    mov $4294967294, %r11
    mov %r11d, 0(%rsp)
    movl $4, 4(%rsp)
    movl $8, 8(%rsp)
    addq $24, %rsp
    ret
