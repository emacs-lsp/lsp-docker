FROM ubuntu:18.04 AS ccls
RUN apt-get update \
	&& apt-get upgrade -y \
	&& apt-get install -y build-essential cmake clang libclang-dev zlib1g-dev git wget \
	&& git clone --depth=1 --recursive https://github.com/MaskRay/ccls \
	&& cd ccls \
	&& wget -c http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz \
	&& tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz \
	&& cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$PWD/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04 \
	&& cmake --build Release

FROM ubuntu:18.04 AS go
RUN apt-get update \
	&& apt-get upgrade -y \
	&& apt-get install -y wget \
	&& wget -c https://dl.google.com/go/go1.13.8.linux-amd64.tar.gz \
	&& tar -xzf go1.13.8.linux-amd64.tar.gz


FROM ubuntu:18.04
# General
RUN apt-get update \
	&& apt-get upgrade -y \ 
	&& apt-get install -y git build-essential

# Emacs
RUN apt-get install -y autoconf texinfo libgnutls28-dev libxml2-dev libncurses5-dev \
	&& git clone --depth=1 git://git.sv.gnu.org/emacs.git /emacs \
	&& cd /emacs && ./autogen.sh \
	&& ./configure --with-modules --with-json \
	&& make -j4 && make install

# C-Family
COPY --from=ccls /ccls /ccls
RUN apt-get install -y clangd-9 \
	&& ln -s `which clangd-9` /usr/bin/clangd \
	&& ln -s /ccls/Release/ccls /usr/bin/ccls

# Go
COPY --from=go /go /go
ENV PATH "${PATH}:/go/bin:/root/go/bin"
RUN /go/bin/go get -u golang.org/x/tools/gopls

# NPM installed language servers
RUN apt-get install -y npm \
	&& npm i -g \
	bash-language-server \
	vscode-css-languageserver-bin \
	dockerfile-language-server-nodejs


#RUN apt-get update && apt-get install -y wget software-properties-common curl
#RUN apt-get install gnupg -y
#RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
#
#RUN add-apt-repository ppa:longsleep/golang-backports
#RUN add-apt-repository ppa:laurent-boulard/fonts
#RUN echo deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic-9 main >> /etc/apt/sources.list
#RUN echo deb-src http://apt.llvm.org/bionic/ llvm-toolchain-bionic-9 main
#RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
#RUN apt-key adv --keyserver hkps://keyserver.ubuntu.com:443 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
#RUN curl -sL https://deb.nodesource.com/setup_11.x | bash -
#
#RUN apt-get update
#RUN apt-get install -y nodejs
#
#ENV TZ=Europe/Minsk
#RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
#
#RUN apt-get install -y g++ build-essential cmake git
#RUN apt-get install -y  libncurses-dev  zlib1g-dev libevent-dev wget python-pip libmsgpack-dev
#RUN apt-get install -y libjsoncpp-dev libmysqlcppconn-dev libgtest-dev cmake libgoogle-perftools-dev
#RUN apt-get install -y libssl-dev libmysqlclient-dev libcrypto++-dev libc6-dev libc6-dev-i386 autoconf
#RUN apt-get install -y texinfo libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev
#RUN apt-get install -y libgnutls28-dev libncurses5-dev libxml2-dev libxt-dev libjansson4 curl
#RUN apt-get install -y unzip clang-tools-8 default-jdk composer  golang-go ruby-dev rubygems
#RUN apt-get install -y fonts-iosevka sbt maven
#
## RUN apt-get remove clang-tools-8 -y
## RUN apt-get install clang-tools-8 -y
## clangd
#
#RUN git clone --depth=1 --recursive https://github.com/MaskRay/ccls
#
## ccls
#RUN cd ccls && wget -c http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz && tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz && cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$PWD/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04 && cmake --build Release
#RUN ln -s /ccls/Release/ccls /usr/bin/
#RUN chmod a+x /usr/bin/ccls
#
## pyls
#RUN pip install python-language-server[all]
#
## rust
#RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rust-install.sh
#RUN sh /tmp/rust-install.sh -y
#RUN ~/.cargo/bin/rustup component add rls rust-analysis rust-src
#RUN sh $HOME/.cargo/env
#
#
## CSS/LessCSS/SASS/SCSS
#RUN npm install -g vscode-css-languageserver-bin
#
## Dockerfile
#RUN npm install -g dockerfile-language-server-nodejs
#
## Elm
#RUN npm i -g @elm-tooling/elm-language-server
#
## HTML
#RUN npm install -g vscode-html-languageserver-bin
#
## JavaScript/TypeScript
#RUN npm i -g typescript-language-server;
#RUN npm i -g typescript
#
## JavaScript/TypeScript
#RUN npm i -g javascript-typescript-langserver
#
## PHP(recommended)
#RUN npm i intelephense -g
#
## Vue
#RUN npm install -g vue-language-server
#
## fortran
#RUN pip install fortran-language-server
#
## bash
## RUN npm config set python /usr/bin/python2
## RUN npm install --unsafe-perm -g node-inspector
## RUN npm i -g bash-language-server
#
## scala
#RUN curl -Lo coursier https://git.io/coursier-cli-linux && \
#	chmod +x coursier
#RUN ./coursier bootstrap \
#	--java-opt -Xss4m \
#	--java-opt -Xms100m \
#	--java-opt -Dmetals.client=emacs \
#	org.scalameta:metals_2.12:0.7.0 \
#	-r bintray:scalacenter/releases \
#	-r sonatype:snapshots \
#	-o /usr/local/bin/metals-emacs -f
#
## GO
#RUN go get -u golang.org/x/tools/gopls
#ENV PATH="/root/go/bin:${PATH}"
#
## ruby
#RUN gem install solargraph
#
## kotlin
#RUN git clone --depth=1 https://github.com/fwcd/kotlin-language-server/ /kotlin-language-server
#RUN cd /kotlin-language-server && ./gradlew :server:installDist
#
## xml langauge server
#RUN wget -O /root/org.eclipse.lsp4xml.jar https://bintray.com/api/ui/download/lsp4xml/releases/org/lsp4xml/org.eclipse.lsp4xml/0.8.0/org.eclipse.lsp4xml-0.8.0-uber.jar
#
## silver searcher
#RUN apt-get install -y silversearcher-ag
#
#
#ENV PATH="/kotlin-language-server/server/build/install/server/bin/:/root/.cargo/bin:${PATH}"
#ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/
#
#COPY local /root/.local
#
#RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && dpkg -i erlang-solutions_1.0_all.deb
#RUN apt-get update && apt-get install -y esl-erlang
#ENV PATH="/rebar3/_build/prod/bin/:${PATH}"
#RUN git clone https://github.com/erlang/rebar3.git && cd /rebar3 && ./bootstrap
#RUN git clone https://github.com/erlang-ls/erlang_ls --depth 1 && cd erlang_ls && rebar3 escriptize
#ENV PATH="/erlang_ls/_build/default/bin/:${PATH}"
#
## csharp
#RUN apt install gnupg ca-certificates
#RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
#RUN echo "deb https://download.mono-project.com/repo/ubuntu stable-bionic main" | tee /etc/apt/sources.list.d/mono-official-stable.list
#RUN apt update
#RUN apt install -y mono-complete
#
#RUN wget -O /tmp/omnisharp.zip https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.34.3/omnisharp-linux-x64.zip
#RUN unzip -d /root/.omnisharp /tmp/omnisharp.zip
#
## haxe
#RUN add-apt-repository ppa:haxe/releases -y
#RUN apt-get update
#RUN apt-get install haxe -y
#RUN mkdir ~/haxelib && haxelib setup ~/haxelib
#
## NPX
#RUN npm install -g npx
#
#RUN useradd -ms /bin/bash lsp
#USER lsp
#WORKDIR /home/lsp
#
#RUN mkdir ~/haxelib && haxelib setup ~/haxelib
#
## haxe language server
#RUN git clone https://github.com/vshaxe/haxe-language-server \
#	&& cd haxe-language-server \
#	&& npm install \
#	&& npx lix run vshaxe-build -t language-server
#
#USER root
#WORKDIR /
#RUN mv /home/lsp/haxe-language-server ~/.haxe-language-server
#
## fasd
#RUN git clone https://github.com/clvv/fasd && cd fasd && make install
#COPY local /home/lsp/.local
#
#RUN  npm install -g elm-test elm-format --unsafe-perm=true --allow-root
#
## elixir
#RUN git clone https://github.com/elixir-lsp/elixir-ls
#WORKDIR /elixir-ls
#RUN apt-get install -y elixir
#RUN mix local.hex --force compile
#RUN mix local.rebar --force
#RUN mix deps.get
#RUN mix elixir_ls.release -o /elixir-release
#ENV PATH="/elixir-release/:${PATH}"
#
#COPY local /root/.local
#
#WORKDIR /
#
## RUST analyzer
#
#RUN git clone https://github.com/rust-analyzer/rust-analyzer
#WORKDIR /rust-analyzer
#RUN rustup update
#RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rust-install.sh
#RUN sh  /tmp/rust-install.sh -y
#RUN rustup default nightly
#RUN rustc --version
#RUN  cargo xtask install --server
#RUN rustc --version
#
#WORKDIR /
#
## powershell
#RUN wget -q https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb
#
## Register the Microsoft repository GPG keys
#RUN  dpkg -i packages-microsoft-prod.deb
#
## Update the list of products
#RUN apt-get update
#
## Enable the "universe" repositories
#RUN add-apt-repository universe
#
## Install PowerShell
#RUN apt-get install -y powershell
#
#RUN ~/.cargo/bin/rustup component add rls rust-analysis rust-src
#
##
#RUN apt-get install -y clang-tools-9 clangd-9
#RUN update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-9 100
