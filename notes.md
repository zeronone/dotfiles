
# OSX

## Sync env vars with GUI apps
$ osxdosync

## Open emacs from pyenv shell
Setting `pyenv local <virtual-env>` is not enough.

```
$ cd <project>
$ pyenv shell <virtualenv-name>
$ echo $PYENV_VERSION
$ emacsgui
```




