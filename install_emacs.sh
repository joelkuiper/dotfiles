#!/bin/bash

sudo apt install -y build-essential gcc-10  libgccjit-10-dev libxpm-dev libjpeg-dev libgif-dev libtiff-dev libgnutls28-dev libjson11-1-dev  gnutls-bin libjansson-dev libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev texinfo libtool-bin libtool libtree-sitter-dev libxml2-dev

git clone --depth 1 --branch emacs-30.2 git@github.com:emacs-mirror/emacs.git

cd ~/emacs
CC="gcc-10" ./autogen.sh
CC="gcc-10" ./configure --with-harfbuzz --with-threads --with-cairo --with-native-compilation CFLAGS="-O2 -pipe -march=native -fomit-frame-pointer"
CC="gcc-10" make -j $(nproc --all)
sudo make install
