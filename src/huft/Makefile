TOP := ../..

BUILD=$(TOP)/build
BIN=$(TOP)/bin

huft:
	cabal build huft && ln -sf `cabal list-bin huft` ../../bin/uft && ln -sf `cabal list-bin huft` ../../bin/huft

clean:
	rm -f $(BIN)/uft
	cabal clean