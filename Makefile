compile:
	ghc tableaux.hs -o tableaux

run:
	ghc example.hs tableaux.hs -o example && ./example