FROM haskell AS builder

WORKDIR /build
COPY . .
RUN apt-get update && apt-get install -y libpq-dev
RUN stack setup
RUN stack install --local-bin-path .

FROM debian
RUN apt-get update && apt-get install -y libpq5 ca-certificates && rm -rf /var/lib/apt/lists/*
COPY --from=builder /build/dbmonitor /
CMD ["/dbmonitor"]
