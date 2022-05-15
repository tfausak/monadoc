FROM ubuntu:22.04

ENV LANG=C.UTF-8

RUN apt-get update && apt-get install --assume-yes ca-certificates

ARG USER_NAME=monadoc
RUN useradd --create-home "$USER_NAME"
USER "$USER_NAME"
WORKDIR "/home/$USER_NAME"

RUN mkdir volume
VOLUME "/home/$USER_NAME/volume"

COPY data data
COPY monadoc /usr/local/bin

ENV MONADOC_BASE=/
ENV MONADOC_DATA="/home/$USER_NAME/data"
ENV MONADOC_HACKAGE=https://hackage.haskell.org/
ENV MONADOC_HOST=*
ENV MONADOC_PORT=8080
ENV MONADOC_SQL="/home/$USER_NAME/volume/monadoc.sqlite"

ARG MONADOC_SHA
ENV MONADOC_SHA="$MONADOC_SHA"

CMD [ "sh", "-c", "exec /usr/local/bin/monadoc \
  --base=$MONADOC_BASE \
  --data=$MONADOC_DATA \
  --hackage=$MONADOC_HACKAGE \
  --host=$MONADOC_HOST \
  --port=$MONADOC_PORT \
  --sql=$MONADOC_SQL" \
  ]
