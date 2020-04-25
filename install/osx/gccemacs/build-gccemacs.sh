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

# patches for emacs-head
patch -g 0 -f -p1 -i $DIR/0001-No-frame-refocus-cocoa.patch
patch -g 0 -f -p1 -i $DIR/0003-Pdumper-size-increase.patch
patch -g 0 -f -p1 -i $DIR/0005-Xwidgets-webkit-in-cocoa-pdumper.patch

git status

# instead usr/include
# sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /


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

    # Required by gnutls
    /usr/local/opt/nettle
    /usr/local/opt/libtasn1
    /usr/local/opt/p11-kit
)

CFLAGS="-g3 "
LDFLAGS=""
PKG_CONFIG_PATH=""
PATH="$(brew --prefix gcc)/$(brew list --versions gcc | tr ' ' '\n' | tail -1)/bin:${PATH}"
for dir in "${libs[@]}"; do
    [[ -d "${dir}/lib" ]] && LDFLAGS="${LDFLAGS}-L${dir}/lib "
    [[ -d "${dir}/include" ]] && CFLAGS="${CFLAGS}-I${dir}/include "
    [[ -d "${dir}/lib/pkgconfig" ]] && PKG_CONFIG_PATH="${PKG_CONFIG_PATH}${dir}/lib/pkgconfig:"
    [[ -d "${dir}/bin" ]] && PATH="${dir}/bin:${PATH}"
done
export CPPFLAGS="${CFLAGS}"
export CFLAGS
export LDFLAGS
export PKG_CONFIG_PATH
export PATH

export LDFLAGS="${LDFLAGS}-L/usr/local/lib/gcc/9 "
export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"

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
--with-nativecomp \
--without-dbus \
--without-imagemagick \
--without-x \
--with-xwidgets \
--with-harfbuzz \
--with-pdumper \
--with-ns \
--disable-ns-self-contained \
--with-gnutls \
--with-modules \
--with-xml2 \
--with-json


function catch_errors() {
    echo "script aborted, because of errors";
    exit $?;
}
trap catch_errors ERR;

make --debug=j -j 8 NATIVE_FAST_BOOT=1 BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 0)"'
# make -j 8 NATIVE_FAST_BOOT=1
make install

# Currently this is failing, also need -g3 in CFLAGS
dsymutil nextstep/Emacs.app/Contents/MacOS/Emacs

rm -rf ${prefixdir}/Emacs.app
cp -rf nextstep/Emacs.app  ${prefixdir}/Emacs.app

# emacs binary is crated at ${prefixdir}/bin
ls -l ${prefixdir}/bin

