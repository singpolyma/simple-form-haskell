GHCFLAGS=-Wall -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.4.2

.PHONY: all shell clean doc install

all: report.html doc dist/build/libHSsimple-form-$(VERSION).a dist/simple-form-$(VERSION).tar.gz

install: dist/build/libHSsimple-form-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: SimpleForm.hs SimpleForm/Digestive.hs SimpleForm/Digestive/Combined.hs SimpleForm/Digestive/Validation.hs SimpleForm/Combined.hs SimpleForm/Validation.hs SimpleForm/Render.hs SimpleForm/Render/XHTML5.hs SimpleForm/Render/Bootstrap3.hs SimpleForm/Digestive/Internal.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/simple-form/index.html README

README: simple-form.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/simple-form/index.html: dist/setup-config SimpleForm.hs SimpleForm/Digestive.hs SimpleForm/Digestive/Combined.hs SimpleForm/Digestive/Validation.hs SimpleForm/Combined.hs SimpleForm/Validation.hs SimpleForm/Render.hs SimpleForm/Render/XHTML5.hs SimpleForm/Render/Bootstrap3.hs SimpleForm/Digestive/Internal.hs
	cabal haddock --hyperlink-source

dist/setup-config: simple-form.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/libHSsimple-form-$(VERSION).a: dist/setup-config SimpleForm.hs SimpleForm/Digestive.hs SimpleForm/Digestive/Combined.hs SimpleForm/Digestive/Validation.hs SimpleForm/Combined.hs SimpleForm/Validation.hs SimpleForm/Render.hs SimpleForm/Render/XHTML5.hs SimpleForm/Render/Bootstrap3.hs SimpleForm/Digestive/Internal.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/simple-form-$(VERSION).tar.gz: README dist/setup-config SimpleForm.hs SimpleForm/Digestive.hs SimpleForm/Digestive/Combined.hs SimpleForm/Digestive/Validation.hs SimpleForm/Combined.hs SimpleForm/Validation.hs SimpleForm/Render.hs SimpleForm/Render/XHTML5.hs SimpleForm/Render/Bootstrap3.hs SimpleForm/Digestive/Internal.hs
	cabal check
	cabal sdist
