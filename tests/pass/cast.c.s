_main:
    subq $56, %rsp
    movsb 0(%rsp), %r11
    mov %r11, 16(%rsp)
    movsb 0(%rsp), %r11
    add $1, %r11
    mov %r11, 24(%rsp)
    movsb 24(%rsp), %r11
    mov %r11b, 0(%rsp)
    movsb 16(%rsp), %r11
    mov %r11, 16(%rsp)
    movsl 16(%rsp), %r11
    mov %r11d, 4(%rsp)
    movsb 0(%rsp), %r11
    mov %r11, 16(%rsp)
    movsl 16(%rsp), %r11
    add $2, %r11
    mov %r11, 16(%rsp)
    movsl 16(%rsp), %r11
    mov %r11d, 8(%rsp)
    mov $0, %r11
    subq $1, %r11
    mov %r11, 16(%rsp)
    movzb 16(%rsp), %r11
    mov %r11d, 12(%rsp)
    addq $56, %rsp
    ret
