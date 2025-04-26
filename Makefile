build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--verbose \
		--mood major \
		--randomize 3 \
		--transform L,P,R,N \
		--duration 4


test:
	cabal test


