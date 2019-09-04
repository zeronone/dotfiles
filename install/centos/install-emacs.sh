#!/bin/sh

# source: http://tamipon.hatenablog.com/entry/2018/06/12/224152

# build dependencies
yum groupinstall "Development Tools" -y
sudo yum install texinfo libXpm-devel giflib-devel libtiff-devel libotf-devel ncurses-devel -y

# download emacs
cd /usr/local/src
sudo wget -nc http://ftp.gnu.org/pub/gnu/emacs/emacs-26.1.tar.gz
sudo tar -df emacs-26.1.tar.gz 2>/dev/null || sudo tar zxvf emacs-26.1.tar.gz

# install autoconf
sudo wget -nc http://ftp.jaist.ac.jp/pub/GNU/autoconf/autoconf-2.69.tar.gz
sudo tar -df autoconf-2.69.tar.gz 2>/dev/null || sudo tar zxvf autoconf-2.69.tar.gz
cd autoconf-2.69
sudo ./configure
sudo make
sudo make install


cd /usr/local/src/emacs-26.1
sudo ./autogen.sh
sudo ./configure --with-gnutls=no --with-x-toolkit=no --with-jpeg=no --with-png=no
sudo make bootstrap
sudo make install



