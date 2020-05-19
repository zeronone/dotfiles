

### Emacs
```bash
# docker build -f Dockerfile.emacs --build-arg UNAME=st21371 --build-arg UID=502 --build-arg GNAME=st21371 --build-arg GID=503 --build-arg UHOME=/Users/st21371  -t arif-emacs .
docker build -f Dockerfile.emacs -t arif-emacs .


# for OSX with user info correctly put
function demacs {
  ip=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
  xhost + $ip
  docker run -ti --name emacs\
   -e DISPLAY=$ip:0 \
   -e UNAME="zeronone" \
   -e GNAME="zeronone" \
   -e UHOME="/Users/zeronone" \
   -e UID="502" \
   -e GID="502" \
   -v /Users/zeronone:/Users/zeronone \
   arif-emacs emacs
}
```



