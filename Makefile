site := dist/build/site/site

_site: $(site)
	$(site) build

$(site): site.hs
	cabal build
