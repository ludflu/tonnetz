build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--verbose \
		--mood major \
		--progression "I-V-vi-IV" \
		--duration 16 \
		--context 10 \
		--play 
#		--midi output.mid
#		--transform R,L,P,L \

test:
	cabal test


