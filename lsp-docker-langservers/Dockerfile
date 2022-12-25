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
	&& export LATEST_VERSION=`wget -qO- https://golang.org/dl | grep -oE go[0-9]+\.[0-9]+\.[0-9]+\.linux-amd64\.tar\.gz | head -n 1` \
	&& wget -c https://dl.google.com/go/$LATEST_VERSION \
	&& tar -xzf $LATEST_VERSION

FROM ubuntu:18.04
# General
RUN apt-get update \
	&& apt-get upgrade -y  \
	&& apt-get install -y git

# C-Family
COPY --from=ccls /ccls /ccls
RUN ln -s /ccls/Release/ccls /usr/bin/ccls \
	&& ln -s /ccls/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04/bin/clangd /usr/bin/clangd

# Go
COPY --from=go /go /go
ENV PATH "${PATH}:/go/bin:/root/go/bin"
RUN /go/bin/go get -u golang.org/x/tools/gopls

# NPM installed language servers
# https://github.com/nodesource/distributions/blob/master/README.md
RUN apt-get update \
	&& apt-get upgrade -y  \
	&& apt-get install -y wget gnupg2 \
  && wget --quiet -O - https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - \
  && VERSION="node_8.x" \
  && DISTRO="bionic" \
  && echo "deb https://deb.nodesource.com/$VERSION $DISTRO main" | tee /etc/apt/sources.list.d/nodesource.list \
  && echo "deb-src https://deb.nodesource.com/$VERSION $DISTRO main" | tee -a /etc/apt/sources.list.d/nodesource.list \
  && apt-get update -y && apt-get -y install nodejs \
	&& npm i -g \
	bash-language-server \
	vscode-css-languageserver-bin \
	vscode-html-languageserver-bin \
	dockerfile-language-server-nodejs \
	typescript-language-server \
	typescript

# Python
RUN apt-get install -y python3-pip \
	&& pip3 install 'python-lsp-server[all]'
