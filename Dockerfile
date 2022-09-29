# FROM snazzybucket/idris2
FROM ubuntu:20.04

# 替换apt-get国内镜像
RUN sed -i "s@http://.*archive.ubuntu.com@http://mirrors.tuna.tsinghua.edu.cn@g" /etc/apt/sources.list \
    && sed -i "s@http://.*security.ubuntu.com@http://mirrors.tuna.tsinghua.edu.cn@g" /etc/apt/sources.list \
    && apt-get update

RUN apt-get -y install curl chezscheme git make gcc

RUN apt-get -y install libgmp3-dev

COPY install.bash .
# RUN echo "chezscheme" | bash -c "$(curl -fsSL https://raw.fastgit.org/stefan-hoeck/idris2-pack/main/install.bash)"
RUN echo "chezscheme" | bash install.bash

ENV PATH="/root/.pack/bin:${PATH}"

# RUN apt-get -y install libturbojpeg0-dev

ENTRYPOINT sleep 100000000