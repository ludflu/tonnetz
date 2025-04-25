build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--mood major \
		--transform L,P,R,H,S,N
#		-r 3


test:
	cabal test


