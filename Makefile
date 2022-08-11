.POSIX:

# Default stuff
default: littleton

doc: doc/Makefile
	make -C doc/

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build/ *.install main.native

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

# Core build
_build/default/bin/main.exe: $(wildcard lib/*.ml) $(wildcard lib/*.mli) bin/main.ml
	dune build

littleton: _build/default/bin/main.exe
	cp _build/default/bin/main.exe littleton

build: littleton

# Compile the core implementation to JavaScript
_build/default/website/site.bc.js: $(wildcard lib/*.ml) $(wildcard lib/*.mli) website/site.ml
	dune build website/site.bc.js --profile release

js: _build/default/website/site.bc.js

# Build the entire website
website: js
	cp -r website _build
	cp littleton _build/website
	cp _build/default/website/site.bc.js _build/website/js
	mkdir -p _build/website/examples
	cp examples/*.json _build/website/examples

# Document generation
doc/%.pdf: doc/%.tex
	cd doc/ && \
	pdflatex $(<F) && \
	pdflatex $(<F) && \
	pdflatex $(<F)

.PHONY: test install uninstall clean doc
