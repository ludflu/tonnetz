build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--verbose \
		--mood major \
		--transform R,L,P,L \
		--duration 16 \
		--context 10 \
		--play \
		--midi output.mid

test:
	cabal test


