FROM haskell:9.2.3-slim
RUN \
  apt-get update && \
  env DEBIAN_FRONTEND=noninteractive apt-get install --assume-yes --no-install-recommends \
    sudo && \
  rm --recursive /var/lib/apt/lists/*
ARG USER_NAME=haskell
RUN \
  useradd --create-home --groups sudo --shell "$( command -v bash )" "$USER_NAME" && \
  echo "$USER_NAME ALL=(ALL) NOPASSWD: ALL" > "/etc/sudoers.d/$USER_NAME"
USER "$USER_NAME"
WORKDIR "/home/$USER_NAME"
ENV PATH="/home/$USER_NAME/.cabal/bin:$PATH"
