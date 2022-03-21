FROM haskell:9.2.2-slim

RUN apt-get update && apt-get install --assume-yes --no-install-recommends \
  nano openssh-client sqlite3 sudo

ARG CABAL_STORE=/cabal-store
RUN mkdir --parents "$CABAL_STORE"
RUN chmod g+w "$CABAL_STORE"
RUN chgrp sudo "$CABAL_STORE"
VOLUME "$CABAL_STORE"

ARG USER_NAME=haskell
RUN useradd --create-home --groups sudo --shell "$( command -v bash )" "$USER_NAME"
RUN echo "$USER_NAME ALL=(ALL) NOPASSWD: ALL" > "/etc/sudoers.d/$USER_NAME"

USER "$USER_NAME"
RUN cabal user-config init --augment "store-dir: $CABAL_STORE"
WORKDIR "/home/$USER_NAME"
ENV PATH="/home/$USER_NAME/.cabal/bin:$PATH"
