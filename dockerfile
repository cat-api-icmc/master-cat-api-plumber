FROM rocker/r-ver:4.3.2

ARG ROOT=/var/www/app

RUN mkdir -p ${ROOT}

COPY / ${ROOT}
COPY scripts/start.sh /start.sh
COPY scripts/requirements.sh /requirements.sh

RUN chmod +x /requirements.sh \ 
    && chmod +x /start.sh

EXPOSE 8080

CMD ["sh", "-c", "/requirements.sh && /start.sh"]
