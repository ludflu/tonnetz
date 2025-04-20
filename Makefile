build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--mood major \
		--transform L,P,R \
		-r 3


test:
	cabal test


