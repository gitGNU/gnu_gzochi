dnl kerberos.m4 --- Support for detecting Kerberos v5 configuration
dnl
dnl Copyright (C) 2013 Julian Graham
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

AC_DEFUN([AC_KERBEROS_V5],[
  AC_MSG_CHECKING(for Kerberos v5)

  krb5_required="$1"
  krb5_prefix="/usr"

  AC_ARG_WITH([krb5],
    [AS_HELP_STRING([--with-krb5=DIR], [use Kerberos v5 installation in DIR])],
    [ krb5_prefix="$withval" ])

  if test -f $krb5_prefix/bin/krb5-config; then
    K5CONFIG="$krb5_prefix/bin/krb5-config"
  elif test -f $krb5_prefix/krb5-config; then
    K5CONFIG="$krb5_prefix/krb5-config"
  fi

  if test "x$K5CONFIG" = "x"; then
    if test "$krb5_required" = "yes"; then
      AC_MSG_ERROR([krb5-config not found at $krb5_with])
    else
      AC_MSG_RESULT([no])
    fi
  else
    KRB5_CFLAGS=`$K5CONFIG --cflags`
    KRB5_LIBS=`$K5CONFIG --libs krb5`
    KRB5_VERSION=`$K5CONFIG --version | head -n 1 | awk '{ print @S|@4 }'`

    AC_SUBST([KRB5_CFLAGS])
    AC_SUBST([KRB5_LIBS])
    AC_SUBST([KRB5_VERSION])

    AC_MSG_RESULT([$KRB5_VERSION])
  fi
])
