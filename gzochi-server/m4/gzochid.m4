dnl gzochid.m4 --- General build support utilities for gzochid
dnl
dnl Copyright (C) 2015 Julian Graham
dnl
dnl This is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.
dnl
dnl This software is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this package.  If not, see <http://www.gnu.org/licenses/>.
dnl

dnl AC_PROBE_LIBS(FUNCTION, SEARCH_LIBS,
dnl               [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
dnl               [OTHER-LIBRARIES])
dnl -------------------------------------------------------
dnl Probe for a library defining FUNC, if it's not already available.
dnl This macro behaves very similarly to AC_SEARCH_LIBS, except that it leaves
dnl LIBS intact. This is useful if you are building multiple artifacts, and you
dnl want to prevent LIBS from filling up with libraries that may not be needed 
dnl by every artifact. Use ACTION-IF-FOUND to add the discovered libraries to
dnl a variable of your choosing; the linker arguments will be in 
dnl ac_cv_probe_FUNC.

AC_DEFUN([AC_PROBE_LIBS],[
  AS_VAR_PUSHDEF([ac_Probe], [ac_cv_probe_$1])
  AC_CACHE_CHECK([for library containing $1], 
    [ac_Probe],
    [ac_func_probe_save_LIBS=$LIBS
     AC_LANG_CONFTEST([AC_LANG_CALL([], [$1])])
     for ac_lib in '' $2; do
       if test -z "$ac_lib"; then
         ac_res="none required"
       else
         ac_res=-l$ac_lib
         LIBS="-l$ac_lib $5 $ac_func_probe_save_LIBS"
       fi
       AC_LINK_IFELSE([], [AS_VAR_SET([ac_Probe], [$ac_res])])
       AS_VAR_SET_IF([ac_Probe], [break])
     done
     AS_VAR_SET_IF([ac_Probe], , [AS_VAR_SET([ac_Probe], [no])])
     rm conftest.$ac_ext
     LIBS=$ac_func_probe_save_LIBS
    ])
    AS_VAR_COPY([ac_res], [ac_Probe])
    AS_IF([test "$ac_res" != no],
      [test "$ac_res" = "none required" || $3],
      [$4])
    AS_VAR_POPDEF([ac_Probe])
])
