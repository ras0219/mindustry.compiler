SHELL = /bin/sh
.SUFFIXES:
.PHONY: check all
#.DELETE_ON_ERROR:

all:

MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --no-builtin-variables

AS:=as
CC:=$(shell which clang)
LD:=${CC}
ASFLAGS:=-target x86_64-apple-darwin20.3.0 -g -c
LDFLAGS:=-target x86_64-apple-darwin20.3.0 -mcpu=x86-64
CFLAGS:= \
    -D FAIL_ON_FIRST \
    -target x86_64-apple-darwin20.3.0 \
    -Wno-parentheses \
    -g

intdir:=int1
outdir:=out1
include ${srcdir}/build/main.mk

int1 out1:
	mkdir $@

all: out1/main out1/runall-test

check: out1/runall-test
	out1/runall-test ${srcdir}

check-lldb: out1/runall-test
	/bin/zsh -c "lldb -o run --arch x86_64 out1/runall-test -- ${srcdir}"

compile_commands.json:
	$(MAKE) -nB | ${srcdir}/build/compilecommands.sh >$@
