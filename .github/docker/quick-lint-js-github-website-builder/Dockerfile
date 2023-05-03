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
        cmake \
        ninja-build \
        python3.7 \
        apache2 \
        rsync \
        zip \
        unzip \
        tar \
        xz-utils \
        nodejs

RUN npm install -g yarn

RUN \
    a2enmod deflate headers && \
    sed -i -e 's/AllowOverride\ None/AllowOverride\ FileInfo/' /etc/apache2/apache2.conf
