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

ENV MONADOC_BASE_URL=/
ENV MONADOC_DATA_DIRECTORY="/home/$USER_NAME/data"
ENV MONADOC_DATABASE_FILE="/home/$USER_NAME/volume/monadoc.sqlite"
ENV MONADOC_HACKAGE_URL=https://hackage.haskell.org/
ENV MONADOC_HOST_PREFERENCE=*
ENV MONADOC_LOG_SEVERITY=debug
ENV MONADOC_PORT_NUMBER=8080
ENV MONADOC_PROXY_SALT=
ENV MONADOC_SENTRY_DSN=

ARG MONADOC_COMMIT_SHA
ENV MONADOC_COMMIT_SHA="$MONADOC_COMMIT_SHA"

CMD /usr/local/bin/monadoc
