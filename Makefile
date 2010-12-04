default: uscheme check

LIBRARY_SOURCES = \
	MicroScheme/Value.hs \
	MicroScheme/Parser.hs \
	MicroScheme/Ast.hs \
	MicroScheme/Eval.hs \
	MicroScheme/Jit.hs

TESTS = \
	MicroScheme/ParserTests.hs \
	MicroScheme/AstTests.hs \
	MicroScheme/EvalTests.hs \
	MicroScheme/JitTests.hs \
	MicroSchemeTests.hs

check: MicroSchemeTests
	./MicroSchemeTests

uscheme: $(LIBRARY_SOURCES) uscheme.hs
	ghc --make $@

MicroSchemeTests: $(LIBRARY_SOURCES) $(TESTS)
	ghc --make $@

original/mandelbrot.pbm: original/mandelbrot.ss
	mzscheme $< > $@

clean:
	rm -f MicroSchemeTests
	rm -f *.hi *.ho
	rm -f MicroScheme/*.hi MicroScheme/*.ho

.PHONY: default check clean
