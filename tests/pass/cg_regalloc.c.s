_f:
    subq $24, %rsp
    movq _a@GOTPCREL(%rip), %r11
    mov (%r11), %r10
    mov 0(%rsp), %rdx
    andq %rdx, %r10
    mov %r10, 16(%rsp)
    mov 16(%rsp), %r11
    mov %r11, 8(%rsp)
    addq $24, %rsp
    ret