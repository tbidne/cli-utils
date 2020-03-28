# build and test
FROM fpco/stack-build:lts-15.0 AS builder
LABEL stage=builder
RUN mkdir -p /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --copy-bins --ghc-options="-Werror" --system-ghc --test :spec :inttest

# make exe
FROM ubuntu:18.04
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y \
  git
COPY --from=builder /root/.local/bin/git-utils /opt/app
ENTRYPOINT ["/opt/app/git-utils"]