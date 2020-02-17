IS_DARWIN=`uname | grep -i darwin`
LOCAL_TIME_VOLUME=`[ -z "$IS_DARWIN" ] &&  echo "-v /etc/localtime:/etc/localtime:ro"`
SYSTEM_BUS_SOCKET_VOLUME=`[ -z "$IS_DARWIN" ] && echo "-v /var/run/dbus/system_bus_socket:/var/run/dbus/system_bus_socket"`

echo $SYSTEM_BUS_SOCKET_VOLUME


docker run -ti --rm -v $('pwd'):/mnt/workspace \
       $LOCAL_TIME_VOLUME \
       -v ~/.ssh/id_rsa:${UHOME}/.ssh/id_rsa:ro \
       -v ~/.gnupg:${UHOME}/.gnupg \
       $SYSTEM_BUS_SOCKET_VOLUME \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       -v /etc/machine-id:/etc/machine-id:ro \
       -v $(pwd)/demo-projects/:/Projects \
       -v $(pwd)/emacs.d/:/root/.emacs.d \
       -v $(pwd)/local/share/fonts:/root/.local/share/fonts/ \
       -v ~/.m2/:/root/.m2 \
       -e DISPLAY=$DISPLAY \
       -e TZ=UA \
       emacslsp/lsp-docker-full:latest emacs $*
