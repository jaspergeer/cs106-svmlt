TOP := ../..

BUILD=$(TOP)/build
BIN=$(TOP)/bin

huft:
	cabal install --installdir=$(BIN)

clean:
	cabal clean
	rm -f $(BIN)/uft