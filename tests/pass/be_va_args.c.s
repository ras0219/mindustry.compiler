_g:
    subq $88, %rsp
    mov %rdi, 0(%rsp)
    mov %rsi, 8(%rsp)
    mov 8(%rsp), %rsi
    leaq 24(%rsp), %rdi
    movsd
    movsl 24(%rsp), %rax
    cmp $48, %eax
    setl %al
    movzx %al, %rax
    mov %rax, 48(%rsp)
    mov 48(%rsp), %rax
    cmp $0, %rax
    jz  L$1
    mov 8(%rsp), %r11
    add $16, %r11
    mov %r11, 48(%rsp)
    mov 48(%rsp), %rsi
    leaq 32(%rsp), %rdi
    movsq
    mov 32(%rsp), %r11
    movsl 24(%rsp), %r10
    add %r10, %r11
    mov %r11, 48(%rsp)
    mov 48(%rsp), %rsi
    leaq 20(%rsp), %rdi
    movsd
    movsl 24(%rsp), %r11
    add $8, %r11
    mov %r11, 48(%rsp)
    movsl 48(%rsp), %r11
    mov 8(%rsp), %r10
    mov %r11d, (%r10)
    jmp L$0
L$1:
    mov 8(%rsp), %r11
    add $8, %r11
    mov %r11, 48(%rsp)
    mov 48(%rsp), %rsi
    leaq 40(%rsp), %rdi
    movsq
    mov 40(%rsp), %rsi
    leaq 20(%rsp), %rdi
    movsd
    mov 40(%rsp), %r11
    add $8, %r11
    mov %r11, 56(%rsp)
    mov 56(%rsp), %r11
    mov 48(%rsp), %r10
    mov %r11, (%r10)
L$0:
    movsl 20(%rsp), %r11
    mov %r11d, 16(%rsp)
    addq $88, %rsp
    ret
_f:
    subq $120, %rsp
    mov %rdi, 8(%rsp)
    mov %rsi, 16(%rsp)
    mov %rdx, 24(%rsp)
    mov %rcx, 32(%rsp)
    mov %r8, 40(%rsp)
    mov %r9, 48(%rsp)
    movl $8, 56(%rsp)
    movl $48, 60(%rsp)
    leaq 128(%rsp), %r11
    mov %r11, 64(%rsp)
    leaq 8(%rsp), %r11
    mov %r11, 72(%rsp)
    mov 8(%rsp), %rdi
    leaq 56(%rsp), %rsi
    movb $0, %al
    callq _g
    movsl 56(%rsp), %rax
    cmp $48, %eax
    setl %al
    movzx %al, %rax
    mov %rax, 88(%rsp)
    mov 88(%rsp), %rax
    cmp $0, %rax
    jz  L$3
    mov 72(%rsp), %r11
    movsl 56(%rsp), %r10
    add %r10, %r11
    mov %r11, 88(%rsp)
    mov 88(%rsp), %rsi
    leaq 84(%rsp), %rdi
    movsd
    movsl 56(%rsp), %r11
    add $8, %r11
    mov %r11, 88(%rsp)
    movsl 88(%rsp), %r11
    mov %r11d, 56(%rsp)
    jmp L$2
L$3:
    mov 64(%rsp), %rsi
    leaq 84(%rsp), %rdi
    movsd
    mov 64(%rsp), %r11
    add $8, %r11
    mov %r11, 88(%rsp)
    mov 88(%rsp), %r11
    mov %r11, 64(%rsp)
L$2:
    movsl 84(%rsp), %r11
    mov %r11d, 80(%rsp)
    movq $2, 0(%rsp)
    mov $0, %rdi
    mov $1, %rsi
    mov $2, %rdx
    mov $2, %rcx
    mov $2, %r8
    mov $2, %r9
    movb $0, %al
    callq _f
    addq $120, %rsp
    ret
