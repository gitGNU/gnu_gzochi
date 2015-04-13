#!/bin/sh

# Copyright (C) 2015 Julian Graham
#
# gzochi is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Usage: tx-client.sh

# This script invokes the gzochi external transaction example application
# client, passing it default arguments for the server endpoint and user 
# identity. It modifies but does not clobber the GUILE_LOAD_PATH env var.

if [ "x$GUILE_LOAD_PATH" = "x" ]; then
    GUILE_LOAD_PATH="../../gzochi-guile-client/src"
else
    GUILE_LOAD_PATH="../../gzochi-guile-client/src:$GUILE_LOAD_PATH"
fi
export GUILE_LOAD_PATH

USERNAME=`whoami`

guile -e main tx-client.scm localhost:8001 $USERNAME
