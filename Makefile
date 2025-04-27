build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--verbose \
		--mood major \
		--transform R,L,P,L \
		--duration 4 \
		--context 10 \
		--midi output.mid

test:
	cabal test


