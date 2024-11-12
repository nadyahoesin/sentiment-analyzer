FROM haskell:latest AS build

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

WORKDIR /app

COPY . .

RUN stack setup --install-ghc
RUN stack build --only-dependencies
RUN stack build

FROM haskell:latest

WORKDIR /app

COPY --from=build /app /app

EXPOSE 8080

CMD ["sh", "-c", "stack exec sentiment-analyzer-exe -- --host 0.0.0.0 --port $PORT"]
