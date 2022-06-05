SHELL = /bin/sh
.SUFFIXES:
#.DELETE_ON_ERROR:

MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --no-builtin-variables

srcdir:=$(abspath ..)

AS:=as
CC:=$(shell which clang)
LD:=${CC}
ASFLAGS:=-target x86_64-apple-darwin20.3.0 -g -c
LDFLAGS:=-target x86_64-apple-darwin20.3.0 -mcpu=x86-64
CFLAGS:= \
    -target x86_64-apple-darwin20.3.0 \
    -Wno-parentheses \
    -g \
    -S \

all: stage3/main

outdir:=stage1
intdir:=stage1
include $(srcdir)/main.mk

CC:=stage1/main
CFLAGS:= -D FAIL_ON_FIRST\
    -I /Library/Developer/CommandLineTools/SDKs/MacOSX11.3.sdk/usr/include \
    -I /Library/Developer/CommandLineTools/usr/lib/clang/13.0.0/include \

outdir:=stage2
intdir:=stage2
include $(srcdir)/main.mk

CC:=stage2/main
CFLAGS:=${CFLAGS}
outdir:=stage3
intdir:=stage3
include $(srcdir)/main.mk

stage1 stage2 stage3:
	mkdir -p $@

clean:
	rm -rf stage1 stage2 stage3

srcs:=${wildcard ${srcdir}/lib/*.c}
basenames:=${basename ${notdir ${srcs}}}
stage3bases:=$(addprefix stage3/,$(basenames))

$(addsuffix .i.diff,$(stage3bases)) $(addsuffix .ast.diff,$(stage3bases)): stage3/%.diff: stage2/% stage3/%
	diff -u $^ >$@ | true

.PHONY: ast.diffs

ast.diffs: $(addsuffix .ast.diff,$(stage3bases))
