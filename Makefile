build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--mood major \
		--transform L,P,R
#		-r 5


test:
	cabal test


