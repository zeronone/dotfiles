#!/bin/sh

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
# patch -g 0 -f -p1 -i $DIR/0001-No-frame-refocus-cocoa.patch
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
    /usr/local/opt/gcc
    /usr/local/opt/libxml2
    /usr/local/opt/giflib
    /usr/local/opt/jpeg
    /usr/local/opt/libtiff
    /usr/local/opt/gnutls
    /usr/local/opt/jansson
    /usr/local/opt/cairo
    /usr/local/opt/harfbuzz

    # Required by gnutls
    /usr/local/opt/nettle
    /usr/local/opt/libtasn1
    /usr/local/opt/p11-kit
)

CFLAGS="-g3 "
LDFLAGS=""
PKG_CONFIG_PATH=""

LDFLAGS="${LDFLAGS}-L/usr/local/lib/gcc/9 "
PATH="$(brew --prefix gcc)/$(brew list --versions gcc | tr ' ' '\n' | tail -1)/bin:${PATH}"
PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"

for dir in "${libs[@]}"; do
    [[ -d "${dir}/lib" ]] && LDFLAGS="${LDFLAGS}-L${dir}/lib "
    [[ -d "${dir}/include" ]] && CFLAGS="${CFLAGS}-I${dir}/include "
    [[ -d "${dir}/lib/pkgconfig" ]] && PKG_CONFIG_PATH="${PKG_CONFIG_PATH}${dir}/lib/pkgconfig:"
done
export CPPFLAGS="${CFLAGS}"
export CFLAGS
export LDFLAGS
export PKG_CONFIG_PATH
export PATH

echo "$CPPFLAGS"
echo "$CFLAGS"
echo "$LDFLAGS"
echo "$PKG_CONFIG_PATH"
echo "$PATH"

./autogen.sh
CC='clang' \
./configure \
--disable-silent-rules \
--prefix=${prefixdir} \
--enable-locallisppath=/usr/local/share/emacs/site-lisp \
--disable-ns-self-contained \
--with-nativecomp \
--with-dbus \
--with-gnutls \
--with-modules \
--with-x \
--with-librsvg \
--with-harfbuzz \
--with-cairo \
--with-pdumper \
--with-xml2 \
--with-xwidgets \
--with-json \
--without-imagemagick
#--with-ns
#--with-cocoa
#--without-x \


function catch_errors() {
    echo "script aborted, because of errors";
    exit $?;
}
trap catch_errors ERR;

# make -j 8 NATIVE_FAST_BOOT=1 BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 0)"'
make -j 8 NATIVE_FAST_BOOT=1
# make -j 8
make install

# Currently this is failing, also need -g3 in CFLAGS
dsymutil nextstep/Emacs.app/Contents/MacOS/Emacs

rm -rf ${prefixdir}/Emacs.app
cp -rf nextstep/Emacs.app  ${prefixdir}/Emacs.app

# emacs binary is crated at ${prefixdir}/bin
ls -l ${prefixdir}/bin

