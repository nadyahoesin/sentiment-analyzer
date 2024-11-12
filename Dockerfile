FROM haskell:8.10.7 AS build

RUN apt-get update && apt-get install -y \
    curl \
    && curl -sSL https://get.haskellstack.org/ | sh

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

WORKDIR /app

COPY stack.yaml package.yaml ./

RUN stack setup --install-ghc \
    && stack build --only-dependencies

COPY . .

RUN stack build --ghc-options="-O2" --fast

FROM debian:buster-slim

WORKDIR /app

COPY --from=build /app/.stack-work/install/aarch64-linux-tinfo6-libc6-pre232/da09576deb28cd8a9e155333060a4dab8ff8b662a23eefafcd84cf1a90af99a4/9.6.6/bin/sentiment-analyzer-exe /app/sentiment-analyzer
COPY --from=build /app/updated_training.csv /app/updated_training.csv

EXPOSE 8080

CMD ["/app/sentiment-analyzer", "--host", "0.0.0.0", "--port", "$PORT"]
