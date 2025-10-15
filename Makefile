compile:
    docker run --rm -v $(PWD):/src -w /src haskell:latest ghc example.hs tableaux.hs -o example

run:
    docker run --rm -v $(PWD):/src -w /src haskell:latest sh -c "ghc example.hs tableaux.hs -o example && ./example"