_foo:
    subq $184, %rsp
    mov %rdi, 0(%rsp)
    leaq 8(%rsp), %rdi
    mov $32, %rcx
    xor %rax, %rax
    rep stosb
    movl $0, 8(%rsp)
    leaq 40(%rsp), %rdi
    mov $136, %rcx
    xor %rax, %rax
    rep stosb
    mov 0(%rsp), %r11
    add $8, %r11
    mov %r11, 176(%rsp)
    leaq 176(%rsp), %r11
    mov %r11, 40(%rsp)
    addq $184, %rsp
    ret
