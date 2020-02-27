FROM emacslsp/lsp-docker-langservers:latest

# Emacs
RUN apt-get update \
	&& apt-get upgrade -y \
	&& apt-get install -y build-essential git autoconf texinfo libgnutls28-dev libxml2-dev libncurses5-dev libjansson-dev \
	&& git clone --depth=1 git://git.sv.gnu.org/emacs.git /emacs \
	&& cd /emacs && ./autogen.sh \
	&& ./configure --with-modules --with-json \
	&& make -j4 && make install


COPY .emacs /root/.emacs
