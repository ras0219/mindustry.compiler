AS:=as
CC:=$(shell which clang)
LD:=${CC}
ASFLAGS:=-target x86_64-apple-darwin20.3.0 -g -c
LDFLAGS:=-target x86_64-apple-darwin20.3.0 -mcpu=x86-64
CFLAGS:= \
    -D FAIL_ON_FIRST \
    -target x86_64-apple-darwin20.3.0 \
    -Wno-parentheses \
    -g \
    -S \

include Makefile.impl

stage2.ast.asm.stderr: stage1/main ../lib/ast.c
	stage1/main -I /opt/src/mindustry.compiler/lib -D FAIL_ON_FIRST -S  /opt/src/mindustry.compiler/lib/ast.c -c -o stage2/ast.asm 2>stage2.ast.asm.stderr

stage3.ast.asm.stderr: stage2/main ../lib/ast.c
	stage2/main -I /opt/src/mindustry.compiler/lib -D FAIL_ON_FIRST -S  /opt/src/mindustry.compiler/lib/ast.c -c -o stage3/ast.asm 2>stage3.ast.asm.stderr

repro: stage2.ast.asm.stderr stage3.ast.asm.stderr
	diff -u stage2.ast.asm.stderr stage3.ast.asm.stderr
