FROM ubuntu:20.04

ENV LANG=C.UTF-8
EXPOSE 3000

ARG USER_NAME=monadoc
RUN useradd --create-home "$USER_NAME"
USER "$USER_NAME"
WORKDIR "/home/$USER_NAME"

RUN mkdir volume
VOLUME "/home/$USER_NAME/volume"

COPY data data
ENV monadoc_datadir="/home/$USER_NAME/data"

COPY monadoc /usr/local/bin

ENTRYPOINT [ "monadoc", "--host=*", "--sql=volume/monadoc.sqlite" ]
