_f:
    subq $56, %rsp
    mov %rdi, 0(%rsp)
    mov %esi, 8(%rsp)
    mov 0(%rsp), %r11
    add $4, %r11
    mov %r11, 40(%rsp)
    mov 40(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 0(%rsp), %r11
    subq $4, %r11
    mov %r11, 40(%rsp)
    mov 40(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 0(%rsp), %r11
    add $4, %r11
    mov %r11, 40(%rsp)
    mov 40(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 40(%rsp), %r11
    mov %r11, 16(%rsp)
    mov 0(%rsp), %r11
    mov %r11, 40(%rsp)
    mov 0(%rsp), %r11
    add $4, %r11
    mov %r11, 48(%rsp)
    mov 48(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 40(%rsp), %r11
    mov %r11, 24(%rsp)
    mov 0(%rsp), %r11
    subq $4, %r11
    mov %r11, 40(%rsp)
    mov 40(%rsp), %r11
    mov %r11, 0(%rsp)
    mov 0(%rsp), %r11
    subq $4, %r11
    mov %r11, 40(%rsp)
    mov 40(%rsp), %r11
    mov %r11, 0(%rsp)
    movsl 8(%rsp), %r11
    subq $1, %r11
    mov %r11, 40(%rsp)
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    subq $1, %r11
    mov %r11, 40(%rsp)
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    add $1, %r11
    mov %r11, 40(%rsp)
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    add $1, %r11
    mov %r11, 40(%rsp)
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    imul $2, %r11
    mov %r11, 40(%rsp)
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %r11
    imul $2, %r11
    mov %r11, 40(%rsp)
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    mov %rdx, 40(%rsp)
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    mov %rdx, 40(%rsp)
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    mov %rax, 40(%rsp)
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movsl 8(%rsp), %rax
    mov $1, %rcx
    cqto
    idivq %rcx
    mov %rax, 40(%rsp)
    movsl 40(%rsp), %r11
    mov %r11d, 8(%rsp)
    movl $1, 32(%rsp)
    mov 32(%rsp), %eax
    mov $2, %rcx
    mov $0, %rdx
    divq %rcx
    mov %rax, 40(%rsp)
    mov 40(%rsp), %r11d
    mov %r11d, 32(%rsp)
    mov 32(%rsp), %eax
    mov $2, %rcx
    mov $0, %rdx
    divq %rcx
    mov %rax, 40(%rsp)
    mov 40(%rsp), %r11d
    mov %r11d, 32(%rsp)
    mov 32(%rsp), %r11d
    imul $2, %r11
    mov %r11, 40(%rsp)
    mov 40(%rsp), %r11d
    mov %r11d, 32(%rsp)
    mov 32(%rsp), %r11d
    imul $2, %r11
    mov %r11, 40(%rsp)
    mov 40(%rsp), %r11d
    mov %r11d, 32(%rsp)
    mov 32(%rsp), %eax
    mov $2, %rcx
    mov $0, %rdx
    divq %rcx
    mov %rdx, 40(%rsp)
    mov 40(%rsp), %r11d
    mov %r11d, 32(%rsp)
    mov 32(%rsp), %eax
    mov $2, %rcx
    mov $0, %rdx
    divq %rcx
    mov %rdx, 40(%rsp)
    mov 40(%rsp), %r11d
    mov %r11d, 32(%rsp)
    addq $56, %rsp
    ret
