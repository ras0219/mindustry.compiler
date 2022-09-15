
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
