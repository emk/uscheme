default: check

LIBRARY_SOURCES = \
	MicroScheme/Value.hs \
	MicroScheme/Parser.hs

TESTS = \
	MicroScheme/ParserTests.hs \
	MicroSchemeTests.hs

check: MicroSchemeTests
	./MicroSchemeTests

MicroSchemeTests: $(LIBRARY_SOURCES) $(TESTS)
	ghc --make $@

original/mandelbrot.pbm: original/mandelbrot.ss
	mzscheme $< > $@

clean:
	rm -f MicroSchemeTests
	rm -f *.hi *.ho
	rm -f MicroScheme/*.hi MicroScheme/*.ho

.PHONY: default check clean
