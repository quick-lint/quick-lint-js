FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update
RUN \
    apt-get install -y --no-install-recommends \
        software-properties-common \
        gnupg \ 
        cmake \
        gcc \
        ninja-build \
        python3.7 \
        python3-pip \
        apache2 \ 
        rsync \ 
        zip \ 
        unzip \
        tar \
        xz-utils \
        curl 

RUN \
    curl 'https://deb.nodesource.com/gpgkey/nodesource.gpg.key' | apt-key add - && \
    apt-add-repository 'deb https://deb.nodesource.com/node_14.x focal main' && \
    sed -n -e 's/^deb /deb-src /p' /etc/apt/sources.list >/etc/apt/sources.list.d/quick-lint-js-sources.list && \
    apt-get install -y --no-install-recommends nodejs && \
    npm install -g yarn 

RUN pip install bs4 requests 

RUN \
    a2enmod deflate headers && \
    sed -i -e 's/AllowOverride\ None/AllowOverride\ FileInfo/' /etc/apache2/apache2.conf
