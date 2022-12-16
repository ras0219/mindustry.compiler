_foo:
    subq $56, %rsp
    mov L_.S0(%rip), %r11d
    movzw L_.S0+4(%rip), %r10
    shl $32, %r10
    or %r10, %r11
    mov 0(%rsp), %r10
    mov %r11d, (%r10)
    mov %r11, %rax
    shr $32, %rax
    mov %ax, 4(%r10)
    leaq 8(%rsp), %rdi
    mov $40, %rcx
    xor %rax, %rax
    rep stosb
    movl $1, 32(%rsp)
    movl $2, 36(%rsp)
    movl $3, 40(%rsp)
    mov L_.S0(%rip), %r11d
    movzw L_.S0+4(%rip), %r10
    shl $32, %r10
    or %r10, %r11
    mov 48(%rsp), %r10
    mov %r11d, (%r10)
    mov %r11, %rax
    shr $32, %rax
    mov %ax, 4(%r10)
    addq $56, %rsp
    ret
