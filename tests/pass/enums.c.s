_main:
    subq $56, %rsp
    movl $26, 0(%rsp)
    movsl 0(%rsp), %rax
    cmp $6, %eax
    sete %al
    movzx %al, %rax
    mov %rax, 32(%rsp)
    movsl 32(%rsp), %r11
    mov %r11d, 4(%rsp)
    mov 4(%rsp), %r11d
    andq $5, %r11
    mov %r11, 32(%rsp)
    mov 32(%rsp), %eax
    cmp $0, %rax
    jz  L$0
L$0:
    mov 4(%rsp), %r11d
    mov %r11, 32(%rsp)
    mov 32(%rsp), %eax
    cmp $5, %eax
    sete %al
    movzx %al, %rax
    mov %rax, 32(%rsp)
    movsl 32(%rsp), %rax
    cmp $0, %rax
    jz  L$1
L$1:
    leaq 8(%rsp), %r11
    mov %r11, 16(%rsp)
    mov 16(%rsp), %rdi
    movl $5, (%rdi)
    mov 16(%rsp), %rsi
    leaq 24(%rsp), %rdi
    movsd
    addq $56, %rsp
    ret