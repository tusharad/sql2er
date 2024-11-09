FROM alpine:3.20

COPY ./sql2er-exe /app/sql2er

RUN apk update && \
    apk add --no-cache libc6-compat \ 
    && apk add --no-cache gmp-dev

WORKDIR /app

ENTRYPOINT [ "./sql2er" ]