build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--mood major \
		--transform L

test:
	cabal test


