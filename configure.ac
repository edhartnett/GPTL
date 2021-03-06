# This is part of the GPTL package. See the COPYING file for
# more information.
# Ed Hartnett, 5/18/18

# Specify minimum autoconf version.
AC_PREREQ([2.59])

# Initialize autoconf.
AC_INIT([GPTL], [5.6.0], [james.rosinski@noaa.gov])

# Find out about the host we're building on.
AC_CANONICAL_HOST

# Find out about the target we're building for.
AC_CANONICAL_TARGET

# Initialize automake.
AM_INIT_AUTOMAKE([foreign subdir-objects])

# Keep libtool macros in an m4 directory.
AC_CONFIG_MACRO_DIR([m4])

# Set up libtool.
LT_PREREQ([2.4])
LT_INIT()

# The config.h file will be created when configure script is run.
AC_CONFIG_HEADERS([config.h])

# Find the C compiler.
AC_PROG_CC()

# These ensure proper handling of const and inline.
AC_C_CONST
AC_C_INLINE

# Set HAVE_NANOTIME on x86 systems only.
AC_MSG_CHECKING([whether x86 nanotime is available])
AS_CASE([$host], [*86*], [have_nanotime=yes], [have_nanotime=no])
if test "x$have_nanotime" = xyes; then
   AC_DEFINE([HAVE_NANOTIME], [1], [x86 nanotime capability is present])
fi
AC_MSG_RESULT($have_nanotime)

# Does the user want to turn on nested OMP? Enable only if you will be
# profiling code regions inside of doubly-nested OpenMP regions. Using
# --enable-nestedomp can increase the cost of getting the thread
# number by about 50% even in regions which are not doubly-nested.
AC_MSG_CHECKING([whether nested OMP is to be enabled])
AC_ARG_ENABLE([nestedomp], [AS_HELP_STRING([--enable-nestedomp],
              [build with nested OMP capability])])
test "x$enable_nestedomp" = xyes || enable_nestedomp=no
AM_CONDITIONAL(ENABLE_NESTEDOMP, [test x$enable_nestedomp = xyes])
AC_MSG_RESULT($enable_nestedomp)

# Does the user want to turn on PMPI?
AC_MSG_CHECKING([whether PMPI is to be enabled])
AC_ARG_ENABLE([pmpi], [AS_HELP_STRING([--enable-pmpi],
              [build with PMPI capability])])
test "x$enable_pmpi" = xyes || enable_pmpi=no
AM_CONDITIONAL(ENABLE_PMPI, [test x$enable_pmpi = xyes])
if test $enable_pmpi = yes; then
   AC_DEFINE([ENABLE_PMPI], [1], [enable pmpi])
fi
AC_MSG_RESULT($enable_pmpi)
AC_DEFINE([MPI_STATUS_SIZE_IN_INTS], [5], [size of status in MPI])

# Does the user want to test autoprofiling? (adds 2 tests to "make
# test"). If compiler is gcc or pathscale or intel, set INSTRFLAG to
# -finstrument-functions. PGI 8.0.2 and later provide
# -Minstrument:functions.
AC_MSG_CHECKING([whether autoprofile testing is to be enabled])
AC_ARG_ENABLE([autoprofile-testing], [AS_HELP_STRING([--enable-autoprofile-testing],
              [test autoprofileing])])
test "x$enable_autoprofile_testing" = xyes || enable_autoprofile_testing=no
AM_CONDITIONAL(TEST_AUTOPROFILE, [test $enable_autoprofile_testing = yes])
AC_MSG_RESULT($enable_autoprofile_testing)

# Does the user want to use double underscores for fortran wrappers?
AC_MSG_CHECKING([whether double underscore for Fortran wrappers should be enabled])
AC_ARG_ENABLE([double-underscore], [AS_HELP_STRING([--enable-double-underscore],
              [use double underscore for Fortran wrappers])])
test "x$enable_double_underscore" = xyes || enable_double_underscore=no
AC_MSG_RESULT($enable_pmpi)
if test $enable_double_underscore = yes; then
   AC_DEFINE([FORTRANDOUBLEUNDERSCORE], [1], [use double underscore for Fortran wrappers])
else
   AC_DEFINE([FORTRANUNDERSCORE], [1], [use single underscore for Fortran wrappers])
fi

# Check for papi library.
AC_CHECK_LIB([papi], [PAPI_library_init])
AC_MSG_CHECKING([whether system can support PAPI])
if test "x$ac_cv_lib_papi_PAPI_library_init" = xyes; then
   # If we have PAPI library, check /proc/sys/kernel/perf_event_paranoid
   # to see if we have permissions.
   if test -f /proc/sys/kernel/perf_event_paranoid; then
      if test `cat /proc/sys/kernel/perf_event_paranoid` != 1; then
         AC_MSG_ERROR([PAPI library found, but /proc/sys/kernel/perf_event_paranoid != 1
                            try sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'])
      fi
   fi
   AC_DEFINE([HAVE_PAPI], [1], [PAPI library is present and usable])
   have_papi=yes
fi
AC_MSG_RESULT($have_papi)
AM_CONDITIONAL([HAVE_PAPI], [test "x$have_papi" = xyes])

# Check for rt library. clock_gettime() in librt.a is an option for
# gathering wallclock time stats on some machines. Setting
# HAVE_LIBRT=yes enables this, but will probably require linking
# applications with -lrt
AC_CHECK_LIB([rt], [clock_gettime])

# Only define HAVE_NANOTIME if this is a x86. It provides by far the finest grained,
# lowest overhead wallclock timer on that architecture.
# If HAVE_NANOTIME=yes, set BIT64=yes if this is an x86_64
dnl HAVE_NANOTIME = yes
dnl ifeq ($(HAVE_NANOTIME),yes)
dnl   BIT64 = yes
dnl endif

# Check for existance of procfs, the proc file system.
AC_CHECK_FILE([/proc],
        [AC_DEFINE([HAVE_SLASHPROC], [1], [some comment])])

# Check for pthread lirbary.
AC_CHECK_LIB([pthread], [pthread_mutex_init])
if test "x$ac_cv_lib_pthread_pthread_mutex_init" = xyes; then
   AC_DEFINE([PTHREADS], [1], [pthreads library is present])
fi

# We need the math library for some tests.
AC_CHECK_LIB([m], [floor], [],
                  [AC_MSG_ERROR([Can't find or link to the math library.])])

# Check for function backtrace_symbols. If HAVE_BACKTRACE=yes will
# enable auto-generation of function name when auto-profiling and
# GPTLdopr_memusage has been enabled at run-time. If
# HAVE_BACKTRACE=no, function address will be printed instead.  GNU
# compilers: compile application code with -finstrument-functions
# -rdynamic Intel compilers: compile application code with
# -finstrument-functions -rdynamic -g
AC_CHECK_FUNC([backtrace_symbols],
        [AC_DEFINE([HAVE_BACKTRACE], [1], [backtrace_symbols function is present])])

# Check for times.
AC_CHECK_FUNC([times],
        [AC_DEFINE([HAVE_TIMES], [1], [vfprint function is available])])

# Check for gettimeofday.
AC_CHECK_FUNC([gettimeofday],
        [AC_DEFINE([HAVE_GETTIMEOFDAY], [1], [gettimeofday function is available])])

# Do we have MPI?
AC_CHECK_FUNC([MPI_Init], [have_mpi=yes])
AM_CONDITIONAL([HAVE_MPI], [test "x$have_mpi" = xyes])

# Do we have function MPI_Comm_f2c?
AC_CHECK_FUNC([MPI_Comm_f2c], [have_mpi_comm_f2c=yes])
if test "x$have_mpi_comm_f2c" = xyes; then
   AC_DEFINE([HAVE_COMM_F2C], [1], [MPI_Comm_f2c is present])
fi

# Check for function iargc, which may be part of MPI.
AC_CHECK_FUNC([iargc],
        [AC_DEFINE([HAVE_IARGCGETARG], [1], [backtrace_symbols function is present])])

# Check the size of a void pointer.
AC_CHECK_SIZEOF([void *])
if test "x$ac_cv_sizeof_void_p" = x8; then
   AC_DEFINE([BIT64], [1], [void pointer is 8 bytes])
fi

# Is doxygen installed?
AC_CHECK_PROGS([DOXYGEN], [doxygen])
if test -z "$DOXYGEN"; then
   AC_MSG_WARN([Doxygen not found - documentation will not be built])
fi

# This is a list of files to be built.
AC_CONFIG_FILES([Makefile
                 include/Makefile
                 ctests/Makefile
                 src/Makefile
                 src/clib/Makefile
                 bin/Makefile
		 ])

# Build the files listed above.
AC_OUTPUT()
