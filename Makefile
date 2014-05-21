site := dist/build/site/site
build_type := build

.PHONY: all
all: _site

.PHONY: diff
diff:
	cd _site && git diff

.PHONY: publish
publish:
	touch _site/.nojekyll
	cd _site && git add .
	cd _site && git commit -am "`date`"
	git fetch _site gh-pages:gh-pages

.PHONY: rebuild
rebuild: build_type = rebuild
rebuild: _site

_site/.git:
	mkdir -p _site
	cd _site && git init

_site: _site/.git
	cd _site && git pull .. +gh-pages:gh-pages
	cd _site && git checkout -f gh-pages
	cabal run $(build_type)

$(site): site.hs
	cabal build
