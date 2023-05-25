_g:
    subq $24, %rsp
    movb $0, %al
    callq _f
    movl $5, 0(%rsp)
    movsl 0(%rsp), %r11
    add $2, %r11
    mov %r11, 8(%rsp)
    movsl 8(%rsp), %r11
    mov %r11d, 4(%rsp)
    movsl 0(%rsp), %r11
    add $3, %r11
    mov %r11, 8(%rsp)
    movsl 8(%rsp), %r11
    mov %r11d, 4(%rsp)
    addq $24, %rsp
    ret
