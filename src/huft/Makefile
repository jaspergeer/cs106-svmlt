TOP := ../..

BUILD=$(TOP)/build
BIN=$(TOP)/bin

huft:
	cabal install --installdir=$(BIN) --overwrite-policy=always

clean:
	cabal clean
	rm -f $(BIN)/uft