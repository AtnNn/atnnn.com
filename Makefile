site := dist/build/site/site
sitegit := cd _site && git
build_mode := build

.PHONY: all
all: _site

.PHONY: diff
diff:
	$(sitegit) diff gh-pages

.PHONY: commit
commit: _site
	test -z "`git status --porcelain`" || ( git status; false)
	$(sitegit) add --all .
	$(sitegit) commit -am "`date` `cd .. && git log -1 HEAD --pretty=format:%H`"
	git fetch _site gh-pages:gh-pages

publish: commit
	git push --all

.PHONY: rebuild
rebuild: build_mode = rebuild
rebuild: _site

.PHONY: watch
watch: _site
	cabal run watch

_site: $(site) $(wildcard about.rst css/* github/* index.html posts/* contact.markdown images/* templates/*)
	cabal run $(build_mode)
	if [[ -e _site/.git ]]; then \
	  $(sitegit) checkout --detach --quiet; \
	else \
	  $(sitegit) init; \
	  echo "../../../.git/objects" > .git/objects/info/alternates; \
	fi
	$(sitegit) fetch .. +gh-pages:gh-pages
	$(sitegit) symbolic-ref HEAD refs/heads/gh-pages
	$(sitegit) reset

$(site): site.hs
	cabal build
	$(eval build_mode := rebuild)