FROM centos:7.9.2009

ENV DEBIAN_FRONTEND noninteractive
ENV TZ=Europe/Bucharest
ENV DISPLAY=host.docker.internal:0.0
ENV SCALA_VERSION 2.13.8

RUN yum update -y && \
    yum install -y make autoconf g++ flex bison verilator git curl wget unzip java-11-openjdk.x86_64 && \
    yum clean all

RUN curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes

RUN ./cs install scala:$SCALA_VERSION && ./cs install scalac:$SCALA_VERSION
RUN echo "export PATH=\"$PATH:/root/.local/share/coursier/bin\"" >> /root/.bashrc
