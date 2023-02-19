_main:
    subq $88, %rsp
    mov $4294967294, %r11
    mov %r11d, 0(%rsp)
    movl $4, 4(%rsp)
    movl $8, 8(%rsp)
    leaq 20(%rsp), %r11
    mov %r11, 32(%rsp)
    leaq 12(%rsp), %r11
    mov %r11, 32(%rsp)
    movl $2, 40(%rsp)
    movl $1, 44(%rsp)
    movsl 0(%rsp), %r11
    mov %r11d, 48(%rsp)
    movsl 0(%rsp), %rax
    cmp $0, %rax
    jz  L$1
    movl $0, 48(%rsp)
L$1:
    movsl 48(%rsp), %rax
    cmp $0, %eax
    setne %al
    movzx %al, %rax
    mov %rax, 56(%rsp)
    movsl 56(%rsp), %rax
    cmp $0, %rax
    jz  L$0
    movl $5, 48(%rsp)
L$0:
    movl $5, 52(%rsp)
    addq $88, %rsp
    ret
