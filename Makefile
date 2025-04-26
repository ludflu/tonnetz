build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--verbose \
		--mood major \
		--transform P,R,L \
		--duration 16

#		--randomize 3 \

test:
	cabal test


