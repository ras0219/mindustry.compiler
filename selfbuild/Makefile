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
