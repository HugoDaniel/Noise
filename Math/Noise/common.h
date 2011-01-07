/* include dependencies */
#ifndef _COMMON_H_
#define _COMMON_H_
/* max and min macros */
#if defined(__GNUC__) && defined(linux)
#  define min(x, y)     ({      \
        __typeof__(x) _x = (x); \
        __typeof__(y) _y = (y); \
        _x < _y ? _x : _y;      \
  })
#  define max(x, y)     ({      \
        __typeof__(x) _x = (x); \
        __typeof__(y) _y = (y); \
        _x > _y ? _x : _y;      \
  })
#else
#  if !defined(WINDOWS_H_INCLUDED) /*
				    * not gcc and no windows.h was included
				    * (because windows.h has its own max/min
				    * macros)
				    */
#  define min(x, y)     ((x) < (y) ? (x) : (y))
#  define max(x, y)     ((x) > (y) ? (x) : (y))
#  endif
#endif  /* not gcc */

/* branch prediction macros */
#ifdef __GNUC__
#  define likely(x)     __builtin_expect(!!(x), 1)
#  define unlikely(x)   __builtin_expect(!!(x), 0)
#else   /* not gcc */
#  define likely(x)     (x)
#  define unlikely(x)   (x)
#  define __attribute__(x)
#endif  /* not gcc */

/* data alignment attributes */
#ifdef __GNUC__
#  define ALIGN(x)	__attribute__((aligned(x)))
#else /* not gcc */
#  define ALIGN(x)	__declspec( align(x) )
#endif

/*
 * Functions marked with __ATTR_CONST do not examine any values except
 * their arguments, and have no effects except the return value. This
 * is a stricter class than __ATTR_PURE (see below), since the function
 * is not allowed to read global memory. Such a function can be subject
 * to common subexpression elimination and loop optimization in GCC.
 *
 * Functions marked with __ATTR_MALLOC behave like malloc: any non-NULL
 * pointer the function returns cannot alias any other pointer valid
 * when the function returns. This helps GCC optimize better in some
 * cases.
 *
 * Functions marked with __ATTR_PURE have no effects except the
 * return value and their return value depends only on the parameters
 * and/or global variables. Such a function can be subject to common
 * subexpression elimination and loop optimization in GCC.
 *
 * Functions marked with __NON_NULL cannot receive a NULL pointer in any
 * of their pointer arguments. With this information, the compiler can
 * check for correctness at compile-time (issuing a warning if -Wnonnull
 * is enabled) and possibly do certain optimizations by disregarding the
 * NULL pointer case.
 */
#if defined(__GNUC__) && __GNUC__ >= 3
#  define __ATTR_CONST  __attribute__((__const__))
#  define __ATTR_MALLOC __attribute__((__malloc__))
#  define __ATTR_PURE   __attribute__((__pure__))
#  define __NON_NULL    __attribute__((__nonnull__))
#else
#  define __ATTR_CONST
#  define __ATTR_MALLOC
#  define __ATTR_PURE
#  define __NON_NULL
#endif

/*
 * If we are building a dll, public includes are exported when BUILD_DLL is present
 */
#if defined(BUILD_DLL)
#	define DLL_EXPORT __declspec(dllexport)
#else
#	if !defined(COUPLED)
#		define DLL_EXPORT __declspec(dllimport)
#	else
#		define DLL_EXPORT
#	endif
#
#endif

#endif /* _COMMON_H_ */
