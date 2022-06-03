# expect predefined:
# - intdir
# - srcdir
# - outdir

srcs:=${wildcard ${srcdir}/lib/*.c}
basenames:=${basename ${notdir ${srcs}}}
ints:=${addprefix ${intdir}/,${basenames}}
asms:=${addsuffix .asm,${ints}}
objs:=${addsuffix .o,${ints}} ${intdir}/main.o
pps:=${addsuffix .i,${ints}}
asts:=${addsuffix .ast,${ints}}

${asms}: ${intdir}/%.asm: ${srcdir}/lib/%.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} $(filter %.c,$^) -c -o $@

${intdir}/main.asm: ${srcdir}/cli/main.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} $(filter %.c,$^) -c -o $@

${pps}: ${intdir}/%.i: ${srcdir}/lib/%.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} -E $(filter %.c,$^) -c -o tmp > $@ 2>&1

${intdir}/main.i: ${srcdir}/cli/main.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} -E $(filter %.c,$^) -c -o tmp > $@ 2>&1

${asts}: ${intdir}/%.ast: ${srcdir}/lib/%.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} --debug-parse $(filter %.c,$^) -c -o tmp > $@

${intdir}/main.ast: ${srcdir}/cli/main.c ${CC} | ${intdir}
	${CC} -I ${srcdir}/lib ${CFLAGS} --debug-parse $(filter %.c,$^) -c -o tmp > $@

${objs}: %.o: %.asm | ${intdir}
	${AS} ${ASFLAGS} $< -o $@


${outdir}/main: ${objs} | ${outdir}
	${LD} ${LDFLAGS} $^ -o $@


${asts} ${intdir}/main.ast ${pps} ${intdir}/main.i ${intdir}/main.asm ${asms}: CC:=${CC}

${asts} ${intdir}/main.ast ${pps} ${intdir}/main.i ${intdir}/main.asm ${asms}: CFLAGS:=${CFLAGS}
