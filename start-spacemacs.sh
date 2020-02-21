IS_DARWIN=`uname | grep -i darwin`
EMACS_D_VOLUME=${EMACS_D_VOLUME:-"-v $(pwd)/spacemacs/:/root/.spacemacs"}
PROJECTS_VOLUME=${PROJECTS_VOLUME:-"-v `pwd`/demo-projects/:/Projects"}
DOCKER_FLAGS=${DOCKER_FLAGS:=""}

if [ -z "$IS_DARWIN" ]; then
    # The following mounts are protected in Darwin and can't be used
    LOCAL_FONTS_VOLUME="-v `pwd`/local/share/fonts:/root/.local/share/fonts/"
    LOCAL_TIME_VOLUME="-v /etc/localtime:/etc/localtime:ro"
    MACHINE_ID_VOLUME="-v /etc/machine-id:/etc/machine-id:ro"
    SYSTEM_BUS_SOCKET_VOLUME="-v /var/run/dbus/system_bus_socket:/var/run/dbus/system_bus_socket"
    # Not used
    X11_VOLUME="-v /tmp/.X11-unix:/tmp/.X11-unix"
fi
TIME_ZONE=${TZ:-Europe/Minsk}

docker run -ti --rm -v $('pwd'):/mnt/workspace \
       $EMACS_D_VOLUME \
       $PROJECTS_VOLUME \
       $LOCAL_TIME_VOLUME \
       $MACHINE_ID_VOLUME \
       $SYSTEM_BUS_SOCKET_VOLUME \
       $X11_VOLUME \
       $DOCKER_FLAGS \
       -e DISPLAY=$DISPLAY \
       -e TZ=$TIME_ZONE \
       emacslsp/lsp-docker-full:latest emacs
