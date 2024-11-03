FROM alpine:3.20

COPY sql2er-exe /app/sql2er

RUN apk add libc6-compat gmp-dev

WORKDIR /app

ENTRYPOINT [ "./sql2er" ]