build:
	cabal build


run:
	cabal run tonnetz -- \
		--key C \
		--verbose \
		--mood major \
		--progression "I-IV-V-I" \
		--duration 1 \
		--context 10 \
		--play  \
		--midi chorus.mid
#		--transform R,L,P,L \

test:
	cabal test


