#define _STR(_s) #_s
#define STR(_s) _STR(_s)

/* join tokens with an understore */
#define JOIN(_a, _b) _a##_##_b
#define JOIN3(_a, _b, _c) _a##_##_b##_##_c

#define CONC(A, B) CONC_(A, B)
#define CONC_(A, B) A##B
#define ID(...) __VA_ARGS__

/* Inserts a macro expension phase between a function and its
arguments, allowing those arguments to expand before being used inside
of the body of the function. */
#define APPLY(_func, _a) _func(_a)
#define APPLY2(_func, _a, _b) _func(_a, _b)
#define APPLY3(_func, _a, _b, _c) _func(_a, _b, _c)
#define APPLY4(_func, _a, _b, _c, _d) _func(_a, _b, _c, _e)
#define APPLY5(_func, _a, _b, _c, _d, _e) _func(_a, _b, _c, _d, _e)
#define APPLY6(_func, _a, _b, _c, _d, _e, _f) _func(_a, _b, _c, _d, _e, _f)
#define APPLY7(_func, _a, _b, _c, _d, _e, _f) _func(_a, _b, _c, _d, _e, _f, _g)

// Variadic macro that returns argument count as first element, then all
// arguments
#define NARGS_IMPL(_1, _2, _3, _4, _5, _6, _7, N, ...) N
#define NARGS(...) NARGS_IMPL(__VA_ARGS__, 7, 6, 5, 4, 3, 2, 1)
#define WITH_COUNT(...) NARGS(__VA_ARGS__), __VA_ARGS__

#define FOR_EACH_0(ACTN, E) E
#define FOR_EACH_1(ACTN, E) ACTN(E)
#define FOR_EACH_2(ACTN, E, ...) ACTN(E), FOR_EACH_1(ACTN, __VA_ARGS__)
#define FOR_EACH_3(ACTN, E, ...) ACTN(E), FOR_EACH_2(ACTN, __VA_ARGS__)
#define FOR_EACH_4(ACTN, E, ...) ACTN(E), FOR_EACH_3(ACTN, __VA_ARGS__)
#define FOR_EACH_5(ACTN, E, ...) ACTN(E), FOR_EACH_4(ACTN, __VA_ARGS__)
#define FOR_EACH_6(ACTN, E, ...) ACTN(E), FOR_EACH_5(ACTN, __VA_ARGS__)
#define FOR_EACH_7(ACTN, E, ...) ACTN(E), FOR_EACH_6(ACTN, __VA_ARGS__)
#define FOR_EACH(ACTN, ...)                                                    \
  CONC(FOR_EACH_, NARGS(__VA_ARGS__))(ACTN, __VA_ARGS__)
