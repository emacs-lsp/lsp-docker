FROM ubuntu:18.04

RUN apt-get update && apt-get install -y wget software-properties-common
RUN apt-get install gnupg -y
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -

RUN add-apt-repository ppa:longsleep/golang-backports
RUN add-apt-repository ppa:laurent-boulard/fonts
RUN echo deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic-8 main >> /etc/apt/sources.list
RUN echo deb-src http://apt.llvm.org/bionic/ llvm-toolchain-bionic-8 main
RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
RUN apt-key adv --keyserver hkps://keyserver.ubuntu.com:443 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823


RUN apt-get update
RUN apt-get install -y nodejs npm

ENV TZ=Europe/Minsk
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get install -y g++ build-essential cmake git \
 libncurses-dev  zlib1g-dev libevent-dev wget python-pip libmsgpack-dev \
 libjsoncpp-dev libmysqlcppconn-dev libgtest-dev cmake libgoogle-perftools-dev \
 libssl-dev libmysqlclient-dev libcrypto++-dev libc6-dev libc6-dev-i386 autoconf \
 texinfo libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev \
 libgnutls28-dev libncurses5-dev libxml2-dev libxt-dev libjansson4 curl nodejs \
 unzip clang-tools-8 default-jdk composer  golang-go ruby-dev rubygems \
 fonts-iosevka sbt maven

# clangd
RUN update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-8 100

RUN git clone --depth=1 --recursive https://github.com/MaskRay/ccls

# ccls
RUN cd ccls && wget -c http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz && tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz && cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$PWD/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04 && cmake --build Release
RUN ln -s  /ccls/Release/ccls /usr/bin/
RUN chmod a+x /usr/bin/ccls

# pyls
RUN pip install python-language-server[all]

# rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rust-install.sh
RUN sh /tmp/rust-install.sh -y
RUN export PATH=$PATH:~/.cargo/bin
RUN ~/.cargo/bin/rustup component add rls rust-analysis rust-src

RUN apt-get -y install npm
RUN apt-get install -y nodejs

# bash
RUN npm i -g bash-language-server

# CSS/LessCSS/SASS/SCSS
RUN npm install -g vscode-css-languageserver-bin

# Dockerfile
RUN npm install -g dockerfile-language-server-nodejs

# Elm
RUN npm i -g @elm-tooling/elm-language-server

# HTML
RUN npm install -g vscode-html-languageserver-bin

# JavaScript/TypeScript
RUN npm i -g typescript-language-server;
RUN npm i -g typescript

# JavaScript/TypeScript
RUN npm i -g javascript-typescript-langserver

# PHP(recommended)
RUN npm i intelephense -g

# Vue
RUN npm install -g vue-language-server

# fortran
RUN pip install fortran-language-server

# scala
RUN curl -L -o coursier https://git.io/coursier
RUN chmod +x coursier
RUN ./coursier bootstrap \
  --java-opt -Xss4m \
  --java-opt -Xms100m \
  --java-opt -Dmetals.client=emacs \
  org.scalameta:metals_2.12:0.7.0 \
  -r bintray:scalacenter/releases \
  -r sonatype:snapshots \
  -o /usr/local/bin/metals-emacs -f

# GO
RUN go get -u golang.org/x/tools/cmd/gopls

# ruby
RUN gem install solargraph

# kotlin
RUN git clone --depth=1 https://github.com/fwcd/kotlin-language-server/ /kotlin-language-server
RUN cd /kotlin-language-server && ./gradlew :server:installDist

# xml langauge server
RUN wget -O /root/org.eclipse.lsp4xml.jar https://bintray.com/api/ui/download/lsp4xml/releases/org/lsp4xml/org.eclipse.lsp4xml/0.8.0/org.eclipse.lsp4xml-0.8.0-uber.jar

# silver searcher
RUN apt-get install -y silversearcher-ag

# compile emacs 27
RUN apt-get install -y libjansson-dev

RUN git clone --depth=1 git://git.sv.gnu.org/emacs.git /emacs
RUN cd /emacs && ./autogen.sh && ./configure --with-modules --with-json && make -j4 && make install

ENV PATH="/kotlin-language-server/server/build/install/server/bin/:/root/.cargo/bin:${PATH}"
ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/

# RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
# RUN echo "deb https://download.mono-project.com/repo/ubuntu bionic/snapshots/5.20 main" | tee /etc/apt/sources.list.d/mono-official-stable.list
# RUN apt-get update
# RUN apt-get -y install mono-devel
# RUN mono --version
# RUN git clone -b feature/lsp-0.13 https://github.com/OmniSharp/omnisharp-roslyn /omnisharp-roslyn
# RUN cd /omnisharp-roslyn && ./build.sh
# RUN
