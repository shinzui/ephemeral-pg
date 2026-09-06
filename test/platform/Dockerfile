FROM debian:trixie
RUN apt-get update && apt-get install -y --no-install-recommends ghc libghc-filelock-dev libghc-temporary-dev libghc-hspec-dev postgresql procps ca-certificates && rm -rf /var/lib/apt/lists/*
ADD https://hackage.haskell.org/package/filelock-0.1.1.9/filelock-0.1.1.9.tar.gz /tmp/filelock.tar.gz
RUN cd /tmp && tar xzf filelock.tar.gz && cd filelock-0.1.1.9 && runghc Setup.hs configure --global && runghc Setup.hs build && runghc Setup.hs install
ENV PATH="/usr/lib/postgresql/17/bin:${PATH}"
WORKDIR /probe
COPY src /probe/src
COPY test/platform/Main.hs /probe/Main.hs
RUN ghc -isrc -XGHC2021 -XBlockArguments -XDerivingStrategies -XDuplicateRecordFields -XNoFieldSelectors -XOverloadedRecordDot -XOverloadedStrings -XLambdaCase -XRecordWildCards Main.hs -o /probe/check
USER postgres
CMD ["/probe/check"]
