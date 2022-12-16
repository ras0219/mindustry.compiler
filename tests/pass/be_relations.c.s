_is_ascii_alphu:
    subq $56, %rsp
    mov %edi, 0(%rsp)
    mov $97, %rax
    movsl 0(%rsp), %rdx
    cmp %edx, %eax
    setle %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 16(%rsp)
    movsl 24(%rsp), %rax
    cmp $0, %rax
    jz  L$1
    movsl 0(%rsp), %rax
    cmp $122, %eax
    setle %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 16(%rsp)
L$1:
    movsl 16(%rsp), %rax
    cmp $0, %eax
    setne %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 12(%rsp)
    movsl 24(%rsp), %rax
    cmp $0, %rax
    jnz  L$2
    mov $65, %rax
    movsl 0(%rsp), %rdx
    cmp %edx, %eax
    setl %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 20(%rsp)
    movsl 24(%rsp), %rax
    cmp $0, %rax
    jz  L$3
    movsl 0(%rsp), %rax
    cmp $90, %eax
    setl %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 20(%rsp)
L$3:
    movsl 20(%rsp), %rax
    cmp $0, %eax
    setne %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 12(%rsp)
L$2:
    movsl 12(%rsp), %rax
    cmp $0, %eax
    setne %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 24(%rsp), %rax
    cmp $0, %rax
    jnz  L$4
    movsl 0(%rsp), %rax
    cmp $95, %eax
    sete %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 8(%rsp)
L$4:
    movsl 8(%rsp), %rax
    cmp $0, %eax
    setne %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 4(%rsp)
    movsl 24(%rsp), %rax
    cmp $0, %rax
    jnz  L$5
    movsl 0(%rsp), %rax
    cmp $32, %eax
    setne %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %r11
    mov %r11d, 4(%rsp)
L$5:
    movsl 4(%rsp), %rax
    cmp $0, %eax
    setne %al
    movzx %al, %rax
    mov %rax, 24(%rsp)
    movsl 24(%rsp), %rax

    addq $56, %rsp
    ret
    addq $56, %rsp
    ret
