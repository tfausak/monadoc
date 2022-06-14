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
ENV MONADOC_SENTRY_DSN=

ARG MONADOC_SHA
ENV MONADOC_SHA="$MONADOC_SHA"

CMD [ "sh", "-c", "exec /usr/local/bin/monadoc \
  --base-url=$MONADOC_BASE_URL \
  --data-directory=$MONADOC_DATA_DIRECTORY \
  --database-file=$MONADOC_DATABASE_FILE \
  --hackage-url=$MONADOC_HACKAGE_URL \
  --host-preference=$MONADOC_HOST_PREFERENCE \
  --log-severity=$MONADOC_LOG_SEVERITY \
  --port-number=$MONADOC_PORT_NUMBER \
  --sentry-dsn=$MONADOC_SENTRY_DSN" \
  ]
