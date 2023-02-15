TOP := ../..

BUILD=$(TOP)/build
BIN=$(TOP)/bin

huft:
	cabal install exe:huft --installdir=$(BIN) --overwrite-policy=always

clean:
	cabal clean
	rm -f $(BIN)/huft