#!/bin/sh
# autogen.sh
#
# Copyright (C) 2014 Julian Graham
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this package.  If not, see <http://www.gnu.org/licenses/>.

echo "Generating build files for libgzochi reference client library."

BASEDIR=`dirname $0`
OLDDIR=$PWD

cd $BASEDIR

# Pre-create the 'm4' directory to house libtool scripts.

if [ ! -d m4 ]; then 
    mkdir m4
    if [ $? != 0 ]; then
	cd $OLDDIR
	exit 1
    fi
fi

# On Mac OS X, the native libtool does not include libtoolize; third-party
# libtool packages typically prefix their binaries with 'g', so try 
# 'glibtoolize' on that platform.

case `uname` in 
    Darwin*) glibtoolize -i ;;
          *) libtoolize -i ;; 
esac
if [ $? != 0 ]; then
    cd $OLDDIR
    exit 1
fi

# Generate build files.

aclocal
if [ $? != 0 ]; then
    cd $OLDDIR
    exit 1
fi

automake -a
if [ $? != 0 ]; then
    cd $OLDDIR
    exit 1
fi

autoconf
if [ $? != 0 ]; then
    cd $OLDDIR
    exit 1
fi

cd $OLDDIR
