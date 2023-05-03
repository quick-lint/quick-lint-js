# Stage 1: build clang-format.
FROM alpine:3.17 AS bootstrap

RUN \
    apk add \
      build-base \
      cmake \
      curl \
      gcc \
      ninja \
      python3 \
      tar \
      xz

WORKDIR /build
RUN curl --location https://github.com/llvm/llvm-project/releases/download/llvmorg-9.0.1/llvm-9.0.1.src.tar.xz | tar xJ && \
    mv /build/llvm-9.0.1.src /build/llvm
RUN curl --location https://github.com/llvm/llvm-project/releases/download/llvmorg-9.0.1/clang-9.0.1.src.tar.xz | tar xJ && \
    mv /build/clang-9.0.1.src /build/clang

RUN cmake \
      -S llvm \
      -B build \
      -G Ninja \
      -DCMAKE_PREFIX_PATH=$HOME/Toolchains/clang-format/ \
      -DCMAKE_BUILD_TYPE=Release \
      -DLLVM_ENABLE_PROJECTS=clang \
      -DLLVM_INCLUDE_TESTS=NO
RUN ninja -C build clang-format

# Stage 2: create a minimal image for GitHub.
FROM alpine:3.17
COPY --from=bootstrap /build/build/bin/clang-format /opt/llvm/bin/clang-format
ENV PATH=/opt/llvm/bin:$PATH

RUN \
    apk add \
      bash \
      libstdc++ \
      git \
      musl
