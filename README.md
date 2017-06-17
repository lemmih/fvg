# Functional Vector Graphics

# Test suite

From the `tests/` directory, run: `runhaskell -i../src runtests.hs`

To run a single test: `runhaskell -i../src runtest.sh [file].fvg`
The output will be stored in `[file].out`.

# Frontend

Clone [reflex-platform](https://github.com/reflex-frp/reflex-platform)

Then run `reflex-platform/work-on ghcjs ./. --command "cabal configure --ghcjs -fSite && cabal build"`
