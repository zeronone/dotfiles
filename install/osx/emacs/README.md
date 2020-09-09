

Copied from:
- https://github.com/jixiuf/vmacs/blob/master/bin/build-gccemacs.sh
- https://gist.github.com/mikroskeem/0a5c909c1880408adf732ceba6d3f9ab


```bash
# build gcc with JIT support
./build-patched-gcc.sh

./build-gccemacs.sh

# compile third-party libs
~/gccemacs/bin/emacs --fg-daemon
~/gccemacs/bin/emacsclient --eval '(native-compile-async "~/.emacs.d/.local/straight/build" 4 t)'

# run gccemacs
~/gccemacs/bin/emacs
```




