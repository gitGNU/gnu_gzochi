# Dockerfile for a gzochi build environment with all dependencies.
#
# Also available on Docker Hub under:
# https://hub.docker.com/r/juliangraham/gzochi-build/

FROM samueldebruyn/debian-git
MAINTAINER Julian Graham "joolean@undecidable.net"

RUN apt-get update
RUN apt-get -y install autoconf automake libtool pkg-config make texinfo
RUN apt-get -y install guile-2.0-dev libglib2.0-dev libmicrohttpd-dev \
    zlib1g-dev libdb-dev
RUN apt-get -y install libglib2.0-dev libncurses5-dev
RUN apt-get -y install libglib2.0-dev libsdl1.2-dev
