_f:
    subq $56, %rsp
    mov %rdi, 0(%rsp)
    mov %esi, 8(%rsp)
    mov 0(%rsp), %r11
    add $4, %r11
    mov %r11, 32(%rsp)
    mov 32(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 0(%rsp), %r11
    subq $4, %r11
    mov %r11, 32(%rsp)
    mov 32(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 0(%rsp), %r11
    add $4, %r11
    mov %r11, 32(%rsp)
    mov 32(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 32(%rsp), %r11
    mov %r11, 16(%rsp)
    mov 0(%rsp), %r11
    mov %r11, 32(%rsp)
    mov 0(%rsp), %r11
    add $4, %r11
    mov %r11, 40(%rsp)
    mov 40(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 32(%rsp), %r11
    mov %r11, 24(%rsp)
    mov 0(%rsp), %r11
    subq $4, %r11
    mov %r11, 32(%rsp)
    mov 32(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 0(%rsp), %r11
    subq $4, %r11
    mov %r11, 32(%rsp)
    mov 32(%rsp), %r11
    mov %r11, 0(%rsp)
    movsl 8(%rsp), %r11
    subq $1, %r11
    mov %r11, 32(%rsp)
    movsl 32(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    subq $1, %r11
    mov %r11, 32(%rsp)
    movsl 32(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    add $1, %r11
    mov %r11, 32(%rsp)
    movsl 32(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    add $1, %r11
    mov %r11, 32(%rsp)
    movsl 32(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    mov %rdx, 32(%rsp)
    movsl 32(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    mov %rdx, 32(%rsp)
    movsl 32(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    mov %rax, 32(%rsp)
    movsl 32(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    mov %rax, 32(%rsp)
    movsl 32(%rsp), %r11
    mov %r11d, 8(%rsp)
    addq $56, %rsp
    ret
