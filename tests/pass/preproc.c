
#if 1 ? 0 : 1
#error 'should not execute'
#endif
#if 0 ? 1 : 0
#error 'should not execute'
#endif
#if 1 ? 1 ? 0 : 1 : 1
#error 'should not execute'
#endif
#if 1 ? 0 ? 1 : 0 : 1
#error 'should not execute'
#endif
#if (3 == 3 && 0 >= 5 || 3 > 3)
#error 'invalid'
#endif

#if 1 + 1 != 2
#error 'addition'
#endif

#if 1 - 1 != 0
#error 'subtract'
#endif

#if 2 * 3 != 6
#error 'multiply'
#endif

#if 21 / 7 != 3
#error 'divide'
#endif

#if 23 % 3 != 2
#error 'remainder'
#endif

#if 1 << 2 != 4
#error 'leftshift'
#endif

#if 4 >> 2 != 1
#error 'rightshift'
#endif
