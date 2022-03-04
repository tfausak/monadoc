FROM ghcr.io/tfausak/docker-haskell:9.2.1

RUN \
  sudo apt-get update && \
  sudo apt-get install --assume-yes --no-install-recommends \
    git \
    less \
    libgmp-dev \
    nano \
    zlib1g-dev

RUN cabal user-config init --augment 'store-dir: /cabal'
RUN \
  sudo mkdir /cabal && \
  sudo chown "$( id --user --name )" /cabal && \
  sudo chgrp "$( id --group --name )" /cabal
VOLUME /cabal
