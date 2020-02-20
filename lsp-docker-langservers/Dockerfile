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
	&& apt-get upgrade -y  \
	&& apt-get install -y git

# C-Family
COPY --from=ccls /ccls /ccls
RUN ln -s /ccls/Release/ccls /usr/bin/ccls

# Go
COPY --from=go /go /go
ENV PATH "${PATH}:/go/bin:/root/go/bin"
RUN /go/bin/go get -u golang.org/x/tools/gopls

# NPM installed language servers
RUN apt-get install -y npm \
	&& npm i -g \
	bash-language-server \
	vscode-css-languageserver-bin \
	vscode-html-languageserver-bin \
	dockerfile-language-server-nodejs \
	typescript-language-server \
	typescript

# Python
RUN apt-get install -y python3-pip \
	&& pip3 install 'python-language-server[all]'


