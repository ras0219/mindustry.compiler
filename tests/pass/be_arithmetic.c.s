_f:
    subq $120, %rsp
    mov %rdi, 0(%rsp)
    mov %esi, 8(%rsp)
    mov 0(%rsp), %r11
    add $4, %r11
    mov %r11, 104(%rsp)
    mov 104(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 0(%rsp), %r11
    subq $4, %r11
    mov %r11, 104(%rsp)
    mov 104(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 0(%rsp), %r11
    add $4, %r11
    mov %r11, 104(%rsp)
    mov 104(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 104(%rsp), %r11
    mov %r11, 16(%rsp)
    mov 0(%rsp), %r11
    mov %r11, 104(%rsp)
    mov 0(%rsp), %r11
    add $4, %r11
    mov %r11, 112(%rsp)
    mov 112(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 104(%rsp), %r11
    mov %r11, 24(%rsp)
    mov 0(%rsp), %r11
    subq $4, %r11
    mov %r11, 104(%rsp)
    mov 104(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 0(%rsp), %r11
    subq $4, %r11
    mov 96(%rsp), %r11
    mov %r11, 0(%rsp)
    movsl 8(%rsp), %r11
    subq $1, %r11
    movsl 88(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    subq $1, %r11
    movsl 80(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    add $1, %r11
    movsl 72(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    add $1, %r11
    movsl 64(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    movsl 56(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    movsl 48(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    movsl 32(%rsp), %r11
    mov %r11d, 8(%rsp)
    addq $120, %rsp
    ret
