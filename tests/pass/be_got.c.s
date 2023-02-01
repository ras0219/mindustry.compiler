_f:
    subq $24, %rsp
    movb $0, %al
    callq _f
    movb $0, %al
    callq *_g(%rip)
    movb $0, %al
    callq _h
    movq _i@GOTPCREL(%rip), %r11
    movb $0, %al
    callq *(%r11)
    leaq _f(%rip), %r11
    mov %r11, _g(%rip)
    mov _g(%rip), %r11
    mov %r11, _g(%rip)
    mov _h@GOTPCREL(%rip), %r11
    mov %r11, _g(%rip)
    movq _i@GOTPCREL(%rip), %r11
    mov (%r11), %r10
    mov %r10, _g(%rip)
    addq $24, %rsp
    ret
