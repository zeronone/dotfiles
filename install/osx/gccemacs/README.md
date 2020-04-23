

Copied from:
- https://github.com/jixiuf/vmacs/blob/master/bin/build-gccemacs.sh
- https://gist.github.com/mikroskeem/0a5c909c1880408adf732ceba6d3f9ab


```bash

# build gcc with JIT support
./build-patched-gcc.sh

./build-gccemacs.sh


# add (setq comp-deferred-compilation t) to your ~/.emacs.d/early-init.el

# run gccemacs
gccemacs
```




