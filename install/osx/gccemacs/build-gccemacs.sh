#!/bin/sh

workingdir="$HOME/src/emacs"

mkdir -p $workingdir
git clone https://github.com/emacs-mirror/emacs $workingdir || true
cd $workingdir
git clean -fdx
git checkout master
git branch -D feature/native-comp || true
git pull
git checkout --track origin/feature/native-comp
git status

# instead usr/include
# sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /


prefix=$HOME/gccemacs
if [ ! -d $prefix ]; then
    echo "Creating gccemacs dir: $prefix"
    mkdir -p $prefix
    mkdir -p $prefix/bin
fi
echo "Will install at ${prefix}"

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

CFLAGS=""
LDFLAGS=""
PKG_CONFIG_PATH=""
PATH="$(brew --prefix gcc)/$(brew list --versions gcc | tr ' ' '\n' | tail -1)/bin:${PATH}"
for dir in "${libs[@]}"; do
    CFLAGS="${CFLAGS}-I${dir}/include "
    LDFLAGS="${LDFLAGS}-L${dir}/lib "
    PKG_CONFIG_PATH="${PKG_CONFIG_PATH}${dir}/lib/pkgconfig:"
    PATH="${PATH}${dir}/bin:"
done
export CPPFLAGS="${CFLAGS}"
export CFLAGS
export LDFLAGS
export PKG_CONFIG_PATH
export PATH

export LDFLAGS="${LDFLAGS}-L/usr/local/lib/gcc/9 "
export PATH=$PATH:/usr/local/opt/gnu-sed/libexec/gnubin

echo "$CPPFLAGS"
echo "$CFLAGS"
echo "$LDFLAGS"
echo "$PKG_CONFIG_PATH"
echo "$PATH"

./autogen.sh
CC='clang' \
./configure \
--disable-silent-rules \
--prefix=${prefix} \
--with-nativecomp \
--with-gnutls \
--without-dbus \
--without-imagemagick \
--without-x \
--with-modules \
--with-xml2
--with-json \
--with-ns
# --enable-locallisppath=/usr/local/share/emacs/site-lisp \
# --disable-ns-self-contained
# --with-x \
# --with-ns \
# --with-xwidgets \


function catch_errors() {
    echo "script aborted, because of errors";
    exit $?;
}
trap catch_errors ERR;

make -j 8 NATIVE_FAST_BOOT=1 BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 0)"'
make install

cd $workingdir
rm -rf ${prefix}/Emacs.app
cp -rf nextstep/Emacs.app  ${prefix}/Emacs.app

# link seems invalid
# emacs: dlopen($HOME/gccemacs/Emacs.app/Contents/MacOS/../lisp/eln-x86_64-apple-darwin19.4.0-3e45ce4a4d4
cd $prefix/Emacs.app
ln -s Resources/lisp lisp

cat << EOF > ${prefix}/bin/emacs
#!/bin/bash
exec ${prefix}/Emacs.app/Contents/MacOS/Emacs "\$@"
EOF

chmod 755 ${prefix}/bin/emacs
rm ${prefix}/bin/emacs || true
ln -s ${prefix}/bin/emacs ~/bin/gccemacs
