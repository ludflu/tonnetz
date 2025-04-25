build:
	cabal build


run:
	cabal run tonnetz -- \
		--key Ds \
		--verbose \
		--mood major \
		--randomize 3 \
		--transform L,P,R,N \
		--duration 1


test:
	cabal test


