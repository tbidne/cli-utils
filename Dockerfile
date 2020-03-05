# build and test
FROM fpco/stack-build:lts-15.0 AS builder
LABEL stage=builder
RUN mkdir -p /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --ghc-options="-Werror" --test --system-ghc

# make exe
FROM ubuntu:18.04
RUN mkdir -p opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y \
  git
COPY --from=builder /opt/build/.stack-work/install/x86_64-linux/7fea63adfe26e95c835361fb032ba3b605ffb9d65e7bf83ead0d3ee19187842b/8.8.2/bin /opt/app
ENTRYPOINT ["/opt/app/git-utils-exe"]