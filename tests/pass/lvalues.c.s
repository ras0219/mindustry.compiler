_f:
    subq $56, %rsp
    movl $1, 0(%rsp)
    movl $2, 4(%rsp)
    mov 8(%rsp), %rdi
    movl $3, (%rdi)
    mov 8(%rsp), %rsi
    leaq 32(%rsp), %rdi
    movsd
    movl $4, 16(%rsp)
    movl $5, 20(%rsp)
    mov 24(%rsp), %rdi
    movl $6, (%rdi)
    mov 24(%rsp), %rsi
    leaq 40(%rsp), %rdi
    movsd
    movl $7, 0(%rsp)
    addq $56, %rsp
    ret
