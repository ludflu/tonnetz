build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--mood major \
		--randomize 5 
#		--transform L,P,R,H,S,N


test:
	cabal test


