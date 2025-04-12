run:
	cabal run tonnetz -- \
		--width 150 \
		--height 150 \
		-o out.svg

test:
	cabal test

build:
	cabal build
