_main:
    subq $56, %rsp
    leaq 0(%rsp), %rdi
    mov $32, %rcx
    xor %rax, %rax
    rep stosb
    movl $0, 0(%rsp)
    addq $56, %rsp
    ret
