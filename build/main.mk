# expect predefined:
# - intdir
# - srcdir
# - outdir

hdrs:=${wildcard ${srcdir}/lib/*.h}
lib_srcs:=${wildcard ${srcdir}/lib/*.c}
srcs:=${lib_srcs} ${srcdir}/cli/main.c ${wildcard ${srcdir}/tests/*.c}
asms:=${patsubst ${srcdir}/%.c,${intdir}/%.s,${srcs}}
objs:=${asms:.s=.o}
lib_objs:=${patsubst ${srcdir}/%.c,${intdir}/%.s,${lib_srcs}}

${intdir}/lib ${intdir}/cli ${intdir}/tests: | ${intdir}
	mkdir $@

${asms}: | ${intdir}/lib ${intdir}/cli ${intdir}/tests

${asms}: ${intdir}/%.s: ${srcdir}/%.c ${CC} ${hdrs}
	${CC} -I ${srcdir}/lib -I ${srcdir}/tests ${CFLAGS} $(filter %.c,$^) -S -o $@

${objs}: %.o: %.s
	${AS} ${ASFLAGS} $< -o $@

${outdir}/main: ${lib_objs} ${intdir}/cli/main.o | ${outdir}
	${LD} ${LDFLAGS} $^ -o $@

${outdir}/runall-test: ${lib_objs} \
		${intdir}/tests/test1.o \
		${intdir}/tests/unittest.o \
		${intdir}/tests/test_interval.o \
		${intdir}/tests/test_json.o \
		| ${outdir}
	${LD} ${LDFLAGS} $^ -o $@
