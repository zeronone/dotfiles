#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

workingdir="$HOME/src/emacs"

mkdir -p $workingdir
git clone https://github.com/emacs-mirror/emacs $workingdir || true
cd $workingdir
git clean -fdx
git checkout -- .
git checkout master
git branch -D feature/native-comp || true
git pull
git checkout --track origin/feature/native-comp

# patches from emacs-head
#patch -g 0 -f -p1 -i $DIR/0001-No-frame-refocus-cocoa.patch
#patch -g 0 -f -p1 -i $DIR/0003-Pdumper-size-increase.patch
#patch -g 0 -f -p1 -i $DIR/0005-Xwidgets-webkit-in-cocoa-pdumper.patch

git status

prefixdir=$HOME/gccemacs
if [ ! -d $prefixdir ]; then
    echo "Creating gccemacs dir: $prefixdir"
    mkdir -p $prefixdir
    mkdir -p $prefixdir/bin
fi
echo "Will install at ${prefixdir}"

libs=(
    /usr/local/opt/openssl@1.1
    /usr/local/opt/texinfo
    /usr/local/opt/gnu-sed
    /usr/local/opt/libgccjit
    /usr/local/opt/libxml2
    /usr/local/opt/gnutls
    /usr/local/opt/jansson
#    /usr/local/opt/giflib
#    /usr/local/opt/cairo
#    /usr/local/opt/harfbuzz
#    /usr/local/opt/jpeg
#    /usr/local/opt/libtiff

    # Required by gnutls
    /usr/local/opt/nettle
    /usr/local/opt/libtasn1
    /usr/local/opt/p11-kit
)

CFLAGS="-g3 -O3 -mtune=native -march=native -fomit-frame-pointer "
# libgccgit lib dir is a bit nested
LDFLAGS="-L/usr/local/opt/libgccjit/lib/gcc/10 "
PKG_CONFIG_PATH=""

PATH="$(brew --prefix gcc@10)/bin:${PATH}"
PATH="$(brew --prefix gnu-sed)/libexec/gnubin:$PATH"

for dir in "${libs[@]}"; do
    [[ -d "${dir}/lib" ]] && LDFLAGS="${LDFLAGS}-L${dir}/lib "
    [[ -d "${dir}/bin" ]] && PATH="${dir}/bin:${PATH}"
    [[ -d "${dir}/include" ]] && CFLAGS="${CFLAGS}-I${dir}/include "
    [[ -d "${dir}/lib/pkgconfig" ]] && PKG_CONFIG_PATH="${PKG_CONFIG_PATH}${dir}/lib/pkgconfig:"
done
export CPPFLAGS="${CFLAGS}"
export CFLAGS
export LDFLAGS
export PKG_CONFIG_PATH=${PKG_CONFIG_PATH%:}
export PATH

echo "CPPFLAGS=$CPPFLAGS"
echo "CFLAGS=$CFLAGS"
echo "LDFLAS=$LDFLAGS"
echo "PKG_CONFIG_PATH=$PKG_CONFIG_PATH"
echo "PATH=$PATH"

export CC="/usr/local/bin/gcc-10"
export CXX="/usr/local/bin/g++-10"

./autogen.sh
./configure \
--disable-silent-rules \
--prefix=${prefixdir} \
--enable-locallisppath=/usr/local/share/emacs/site-lisp \
--disable-ns-self-contained \
--without-imagemagick \
--without-ns \
--with-native-compilation \
--with-dbus \
--with-gnutls \
--with-modules \
--with-pdumper \
--with-xml2 \
--with-json \
--with-x-toolkit=no --with-xpm=no --with-jpeg=no --with-png=no --with-gif=no --with-tiff=no  # give up GUI
#--with-librsvg \
#--with-harfbuzz \
#--with-cairo
#--with-x \
#--with-ns                # can't use gcc-10, only clang
#--with-cocoa
#--with-xwidgets \


function catch_errors() {
    echo "script aborted, because of errors";
    exit $?;
}
trap catch_errors ERR;

# make -j 8 NATIVE_FAST_BOOT=1 BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 0)"'
make -j 8 NATIVE_FAST_BOOT=1
# make -j 8
make install

#### GUI
# Currently this is failing, also need -g3 in CFLAGS
#dsymutil nextstep/Emacs.app/Contents/MacOS/Emacs
#
#rm -rf ${prefixdir}/Emacs.app
#cp -rf nextstep/Emacs.app  ${prefixdir}/Emacs.app

# emacs binary is crated at ${prefixdir}/bin
ls -l ${prefixdir}/bin

