_h:
    subq $120, %rsp
    mov %rdi, 0(%rsp)
    mov %rsi, 8(%rsp)
    mov %rdx, 16(%rsp)
    mov %rcx, 24(%rsp)
    mov %r8, 32(%rsp)
    mov %r9, 40(%rsp)
    movl $8, 48(%rsp)
    movl $48, 52(%rsp)
    leaq 128(%rsp), %r11
    mov %r11, 56(%rsp)
    leaq 0(%rsp), %r11
    mov %r11, 64(%rsp)
    leaq 48(%rsp), %rsi
    leaq 72(%rsp), %rdi
    mov $24, %rcx
    cld
    rep movsb
    addq $120, %rsp
    ret
