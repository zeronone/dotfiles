
# Taken from: https://gitlab.com/koral/emacs-nativecomp-dockerfile/-/blob/master/Dockerfile
FROM ubuntu:bionic as emacs-build

RUN apt-get update
RUN apt-get install -y git autoconf texinfo binutils flex bison \
      libmpc-dev libmpfr-dev libgmp-dev coreutils make \
      libtinfo5 texinfo libjpeg-dev libtiff-dev libgif-dev libxpm-dev \
      libgtk-3-dev libgnutls28-dev libncurses5-dev libxml2-dev libxt-dev \
      libjansson4 gcc-multilib g++-8 libcanberra-gtk3-module libjansson-dev
RUN rm /usr/bin/g++ && ln -s /usr/bin/g++-8 /usr/bin/g++

WORKDIR /
ARG GCC_BRANCH=releases/gcc-9
RUN git clone --depth=1 git://gcc.gnu.org/git/gcc.git \
      -b $GCC_BRANCH gcc
RUN git clone --depth=1 git://git.sv.gnu.org/emacs.git \
      -b feature/native-comp emacs-native
RUN mkdir /gcc/build
WORKDIR /gcc/build
RUN ../configure --enable-host-shared --enable-languages=jit \
      --disable-bootstrap --enable-checking=release --prefix=/install_dir
RUN make -j$(nproc)
RUN make install-strip

ENV PATH="/install_dir/bin/:${PATH}"
ENV LD_LIBRARY_PATH=/install_dir/lib
ENV LIBRARY_PATH=/install_dir/lib

WORKDIR /emacs-native/
RUN ./autogen.sh
RUN ./configure --with-nativecomp --with-modules --with-json --prefix=/install_dir
RUN make -j$(nproc) NATIVE_FAST_BOOT=1
RUN make install-strip


# taken from: https://github.com/JAremko/docker-emacs
FROM ubuntu:bionic as app
MAINTAINER Arif Rezai <me@arifrezai.com>

# Fix "Couldn't register with accessibility bus" error message
ENV NO_AT_BRIDGE=1

ENV DEBIAN_FRONTEND noninteractive

# basic stuff
RUN echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf \
    && apt-get update && apt-get install \
    bash \
    build-essential \
    dbus-x11 \
    fontconfig \
    git \
    gzip \
    language-pack-en-base \
    libgl1-mesa-glx \
    make \
    sudo \
    tar \
    unzip \
# below were taken from andreacorallo/emacs-nativecomp
    libmpc3 libmpfr6 libgmp10 coreutils libjpeg62-dev \
    libtiff5 libgif7 libxpm4 libgtk-3-0 libgnutlsxx28 libncurses5 libxml2 \
    libxt6 libjansson4 libcanberra-gtk3-module libx11-xcb1 binutils libc6-dev \
# su-exec
    && git clone https://github.com/ncopa/su-exec.git /tmp/su-exec \
    && cd /tmp/su-exec \
    && make \
    && chmod 770 su-exec \
    && mv ./su-exec /usr/local/sbin/ \
# Cleanup
    && apt-get purge build-essential \
    && apt-get autoremove \
    && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*


# ^^^^^^^ Those layers are shared ^^^^^^^

# Emacs
COPY --from=emacs-build /install_dir /install_dir

# final-stage
FROM app

ARG UNAME="emacs"
ARG GNAME="emacs"
ARG UHOME="/home/emacs"
ARG UID="1000"
ARG GID="1000"


ENV UNAME="${UNAME}" \
    GNAME="${GNAME}" \
    UHOME="${UHOME}" \
    UID="${UID}" \
    GID="${GID}" \
    SHELL="/bin/bash" \
# from emacs-ntive
    PATH="/install_dir/bin/:${PATH}" \
    LD_LIBRARY_PATH="/install_dir/lib" \
    LIBRARY_PATH="/install_dir/lib"

# common dev dependencies
RUN apt-get update \
    && apt-get install curl \
    && curl -LO https://github.com/BurntSushi/ripgrep/releases/download/12.0.1/ripgrep_12.0.1_amd64.deb \
    && sudo dpkg -i ripgrep_12.0.1_amd64.deb \
    && git clone https://github.com/pyenv/pyenv.git ${UHOME}/.pyenv.ubuntu \
    && git clone https://github.com/pyenv/pyenv-virtualenv.git ${UHOME}/.pyenv.ubuntu/plugins/pyenv-virtualenv \
    && echo 'export PYENV_ROOT="$HOME/.pyenv.ubuntu"' >> ${UHOME}/.bashrc \
    && echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ${UHOME}/.bashrc \
    && echo 'eval "$(pyenv init -)"' >> ${UHOME}/.bashrc \
    && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*

RUN mkdir -p /Users \
    && groupadd -g ${GID} ${GNAME} \
    && useradd -rm \
        -d ${UHOME} -s /bin/bash \
        -g ${GID} -G ${GNAME} \
        -u ${UID} ${UNAME} \
    && echo "${UNAME} ALL=(ALL) NOPASSWD: ALL" > "/etc/sudoers.d/${UNAME}" \
    && chmod 0440 "/etc/sudoers.d/${UNAME}" \
    && chown "${UID}":"${GID}" -R ${UHOME}

ENV PATH="${UHOME}/.emacs.d/bin:${PATH}"

# env vars are not accessible in shell-form
# exec form ignores cmd
ENTRYPOINT ["bash", "-c", "su-exec ${UNAME} $0 $1 $2 $3 $4 $5 $6"]
CMD ["emacs"]
