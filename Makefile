site := dist/build/site/site
sitegit := cd _site && git

.PHONY: all
all: _site

.PHONY: diff
diff:
	$(sitegit) diff

.PHONY: publish
publish: _site
	$(sitegit) add .
	$(sitegit) commit -am "`date` `cd .. && git log -1 HEAD --pretty=format:%H`"
	git fetch _site gh-pages:gh-pages

.PHONY: rebuild
rebuild: _site
	

_site: $(site) $(wildcard about.rst css/* github/* index.html posts/* contact.markdown images/* templates/*)
	cabal run rebuild
	$(sitegit) init	
	$(sitegit) fetch .. +gh-pages:gh-pages
	$(sitegit) symbolic-ref HEAD refs/heads/gh-pages
	$(sitegit) reset


$(site): site.hs
	cabal build
