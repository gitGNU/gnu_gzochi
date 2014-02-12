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

echo "Generating build files for mazewar example game."

BASEDIR=`dirname $0`
OLDDIR=$PWD

cd $BASEDIR

# Invoke sub-module autogen.sh script.

./client/autogen.sh
if [ $? != 0 ]; then
    cd $OLDDIR
    exit 1
fi

# Generate build files for the current module.

aclocal
if [ $? != 0 ]; then
    cd $OLDDIR
    exit 1
fi

# Don't force the re-traversal of Makefiles mentioned in configure.ac.

automake -a --no-force
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
