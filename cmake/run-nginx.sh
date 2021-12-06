#!/bin/bash

set -e

CONTAINER_ID=$(basename $(cat /proc/self/cpuset))
VOLUMES=$(docker inspect -f '{{ range $element := .Mounts }}{{.Name}}{{":"}}{{.Destination}}{{"\n"}}{{end}}' $CONTAINER_ID)
MOUNTPOINT=$(df --output=target $1 | tail -n1)
SUBDIR=$(realpath -m "--relative-to=$MOUNTPOINT" $1)
VOLUME=""
IFS=$'\n'
for item in $VOLUMES
do
    IFS=$':'
    read -r name mount < <(printf '%s\n' "$item")
    if [ "$mount" == "$MOUNTPOINT" ]
    then
        VOLUME="$name"
    fi
done

docker run -it --rm \
    --mount type=volume,src=$VOLUME,dst=/data,readonly \
    -p 8080:80 \
    nginx:latest \
    /bin/bash -c "rm -rf /usr/share/nginx/html; ln -ds /data/$SUBDIR /usr/share/nginx/html; nginx -c /usr/share/nginx/html/nginx.conf"
