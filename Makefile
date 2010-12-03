default: check

check: MicroSchemeTests
	./MicroSchemeTests

MicroSchemeTests: MicroScheme.hs MicroSchemeTests.hs
	ghc --make $@

original/mandelbrot.pbm: original/mandelbrot.ss
	mzscheme $< > $@
