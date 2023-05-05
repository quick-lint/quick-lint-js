FROM ubuntu:20.04

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      curl \
      gnupg \
      software-properties-common && \
    curl 'https://deb.nodesource.com/gpgkey/nodesource.gpg.key' | apt-key add - && \
    apt-add-repository 'deb https://deb.nodesource.com/node_14.x focal main' && \
    sed -n -e 's/^deb /deb-src /p' /etc/apt/sources.list >/etc/apt/sources.list.d/quick-lint-js-sources.list

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        apache2 \
        cmake \
        ninja-build \
        nodejs \
        python3.7 \
        rsync \
        tar \
        unzip \
        xz-utils \
        zip

RUN npm install -g yarn

RUN \
    a2enmod deflate headers && \
    sed -i -e 's/AllowOverride\ None/AllowOverride\ FileInfo/' /etc/apache2/apache2.conf

ARG EMSDK_VERSION=3.1.37
ARG EMSCRIPTEN_VERSION=2.0.4
RUN \
    cd /tmp && \
    curl --location https://github.com/emscripten-core/emsdk/archive/${EMSDK_VERSION}.tar.gz | tar xz && \
    mkdir -p /opt && \
    mv /tmp/emsdk-${EMSDK_VERSION} /opt/emsdk && \
    /opt/emsdk/emsdk install ${EMSCRIPTEN_VERSION} && \
    /opt/emsdk/emsdk activate ${EMSCRIPTEN_VERSION}
ENV \
    EMSDK="/opt/emsdk" \
    EMSDK_NODE="/opt/emsdk/node/15.14.0_64bit/bin/node" \
    PATH="/opt/emsdk:/opt/emsdk/upstream/emscripten:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
# Fill Emscripten's C++ standard library build cache.
RUN \
    echo 'int main(){}' >/tmp/cache-populator.cpp && \
    em++ -o /tmp/cache-populator.wasm /tmp/cache-populator.cpp -flto -Os && \
    rm /tmp/cache-populator.cpp /tmp/cache-populator.wasm
