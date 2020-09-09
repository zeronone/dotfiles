#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

workingdir="$HOME/src/emacs-mac"

mkdir -p $workingdir
git clone https://bitbucket.org/mituharu/emacs-mac.git $workingdir || true
cd $workingdir
git clean -fdx
git checkout -- .
git checkout master
git branch -D work || true
git pull
git checkout --track origin/work

git status

prefixdir=$HOME/emacs-mac
if [ ! -d $prefixdir ]; then
    echo "Creating emacs-mac dir: $prefixdir"
    mkdir -p $prefixdir
    mkdir -p $prefixdir/bin
    mkdir -p $prefixdir/app
    mkdir -p $prefixdir/share/emacs/site-lisp
fi
echo "Will install at ${prefixdir}"

libs=(
    /usr/local/opt/openssl@1.1
    /usr/local/opt/texinfo
    /usr/local/opt/gnu-sed
    /usr/local/opt/libxml2
    /usr/local/opt/imagemagick
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

CFLAGS="-g -O3 -fobjc-arc "
LDFLAGS=""
PKG_CONFIG_PATH=""

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
CC="clang" \
./configure \
--disable-silent-rules \
--prefix=${prefixdir} \
--disable-ns-self-contained \
--enable-locallisppath="${prefixdir}/share/emacs/site-lisp" \
--enable-link-time-optimization \
--enable-mac-app=${prefixdir}/app \
--with-mac \
--with-mailutils \
--with-gnutls \
--with-modules \
--with-rsvg \
--with-pdumper \
--with-xml2 \
--with-json \
--with-imagemagick \
--with-dbus \
--without-x

# Adding the following breaks the build
# --with-ns \


function catch_errors() {
    echo "script aborted, because of errors";
    exit $?;
}
trap catch_errors ERR;

make -j 8
make install

echo "Finished"
