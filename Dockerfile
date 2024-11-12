FROM haskell:latest

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

WORKDIR /app

COPY . .

RUN stack setup --install-ghc

RUN stack build --only-dependencies

RUN stack build

EXPOSE 8080

CMD ["stack", "exec", "sentiment-analyzer-exe", "--", "--host", "0.0.0.0", "--port", "8080"]