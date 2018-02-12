FROM haskell:8.0.2 as build
WORKDIR /build

COPY stack.yaml .
RUN stack setup

COPY haskell-sudoku.cabal .
RUN mkdir src test && \
    stack build --dependencies-only --test

COPY src ./src
COPY test ./test
RUN stack build --test && \
    stack install

FROM fpco/haskell-scratch:integer-gmp
COPY --from=build /root/.local/bin/haskell-sudoku /
CMD ["./haskell-sudoku"]
