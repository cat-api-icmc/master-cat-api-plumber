FROM rocker/r-ver:4.3.2

RUN apt-get update \
    && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev libxml2-dev \
    libgit2-dev libssh2-1-dev

COPY scripts/requirements.sh /requirements.sh
RUN chmod +x /requirements.sh && /requirements.sh

ARG ROOT=/var/www/app

RUN mkdir -p ${ROOT}
ADD . ${ROOT}
WORKDIR ${ROOT}

ARG PLUMBER_PORT
ENV PLUMBER_PORT=$PLUMBER_PORT
EXPOSE $PLUMBER_PORT

ENTRYPOINT ["Rscript", "main.R"]
