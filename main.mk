# expect predefined:
# - intdir
# - srcdir
# - outdir

srcs:=${wildcard ${srcdir}/lib/*.c}
basenames:=${basename ${notdir ${srcs}}}
ints:=${addprefix ${intdir}/,${basenames}}
asms:=${addsuffix .asm,${ints}}
objs:=${addsuffix .o,${ints}}
pps:=${addsuffix .i,${ints}}
asts:=${addsuffix .ast,${ints}}

${asms}: ${intdir}/%.asm: ${srcdir}/lib/%.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} $(filter %.c,$^) -c -o $@

${intdir}/main.asm: ${srcdir}/cli/main.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} $(filter %.c,$^) -c -o $@

${intdir}/test1.asm: ${srcdir}/tests/test1.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} $(filter %.c,$^) -c -o $@

${pps}: ${intdir}/%.i: ${srcdir}/lib/%.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} -E $(filter %.c,$^) -c -o tmp > $@ 2>&1

${intdir}/main.i: ${srcdir}/cli/main.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} -E $(filter %.c,$^) -c -o tmp > $@ 2>&1

${intdir}/test1.i: ${srcdir}/tests/test1.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} -E $(filter %.c,$^) -c -o tmp > $@ 2>&1

${asts}: ${intdir}/%.ast: ${srcdir}/lib/%.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} --debug-parse $(filter %.c,$^) -c -o tmp > $@

${intdir}/main.ast: ${srcdir}/cli/main.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} --debug-parse $(filter %.c,$^) -c -o tmp > $@

${intdir}/test1.ast: ${srcdir}/tests/test1.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} --debug-parse $(filter %.c,$^) -c -o tmp > $@

${objs} ${intdir}/main.o ${intdir}/test1.o: %.o: %.asm | ${intdir}
	${AS} ${ASFLAGS} $< -o $@


${outdir}/main: ${objs} ${intdir}/main.o | ${outdir}
	${LD} ${LDFLAGS} $^ -o $@

${outdir}/runall-test: ${objs} ${intdir}/test1.o | ${outdir}
	${LD} ${LDFLAGS} $^ -o $@

files:=${asts} ${intdir}/main.ast ${intdir}/test1.ast \
	${pps} ${intdir}/main.i ${intdir}/test1.i \
	${asms} ${intdir}/main.asm ${intdir}/test1.asm

${files}: CC:=${CC}

${files}: CFLAGS:=${CFLAGS}
