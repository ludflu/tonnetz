build:
	cabal build


run:
	cabal run tonnetz -- \
		--width 1500 \
		--height 1500 \
		-o out.svg

test:
	cabal test


