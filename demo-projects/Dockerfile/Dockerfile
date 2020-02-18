FROM alpine:edge AS builder
RUN echo "BUILD" > /tmp/test

FROM alpine:edge
COPY --from=builder /tmp/test /tmp/test
