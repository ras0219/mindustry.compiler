_dbl2:
    subq $24, %rsp
    addq $24, %rsp
    ret
    addq $24, %rsp
    ret
_square:
    subq $24, %rsp
    mov %edi, 0(%rsp)
    movq _b@GOTPCREL(%rip), %r11
    leaq (%r11), %rsi
    mov _a@GOTPCREL(%rip), %rdi
    mov $3, %rcx
    cld
    rep movsb
    movq _b@GOTPCREL(%rip), %r11
    movq _c@GOTPCREL(%rip), %r10
    leaq (%r11), %rsi
    mov (%r10), %rdi
    mov $3, %rcx
    cld
    rep movsb
    movq _c@GOTPCREL(%rip), %r11
    mov (%r11), %rsi
    leaq 8(%rsp), %rdi
    mov $3, %rcx
    cld
    rep movsb
    movb $0, %al
    callq _dbl
    movb $0, %al
    callq _dbl2
    movb $0, %al
    callq _dbl3
    addq $24, %rsp
    ret
