_main:
    subq $88, %rsp
    leaq 8(%rsp), %rdi
    movb $0, %al
    callq _f
    movsl 12(%rsp), %r11
    mov %r11d, 0(%rsp)
    movb $0, %al
    callq _g
    mov %rax, 56(%rsp)
    movsl 60(%rsp), %r11
    mov 24(%rsp), %r10
    mov %r11d, (%r10)
    mov 24(%rsp), %rsi
    leaq 32(%rsp), %rdi
    movsd
    leaq 40(%rsp), %rdi
    movb $0, %al
    callq _f
    movsl 48(%rsp), %rax
    addq $88, %rsp
    ret
    addq $88, %rsp
    ret
