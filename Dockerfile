FROM haskell AS builder

WORKDIR /build
COPY . .
RUN apt-get update && apt-get install -y libpq-dev
RUN stack setup
RUN stack install --local-bin-path .

FROM scratch
COPY --from=builder /build/dbmonitor /dbmonitor
