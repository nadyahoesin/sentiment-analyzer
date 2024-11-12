FROM haskell:8.10.7 AS build

RUN apt-get update && apt-get install -y \
    curl \
    && curl -sSL https://get.haskellstack.org/ | sh -s - -f \
    && apt-get clean

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

COPY --from=build /app/.stack-work/install/x86_64-linux-*/*/9.6.6/bin/sentiment-analyzer-exe /app/sentiment-analyzer
COPY --from=build /app/updated_training.csv /app/updated_training.csv

EXPOSE 8080
CMD ["/app/sentiment-analyzer", "--host", "0.0.0.0", "--port", "$PORT"]
