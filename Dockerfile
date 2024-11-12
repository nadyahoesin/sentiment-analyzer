FROM haskell:latest

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

WORKDIR /app

COPY . .

RUN stack setup --install-ghc

RUN stack build --only-dependencies

RUN stack build

CMD ["stack", "exec", "sentiment-analyzer-exe"]
