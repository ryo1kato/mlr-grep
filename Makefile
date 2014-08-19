ifeq ($(MAKECMDGOALS),prof)
    OPTS = -rtsopts -prof -auto-all
endif
ifeq ($(MAKECMDGOALS),static)
    OPTS = -static -optl-static -optl-pthread
endif

all: hmlgrep
prof static: all

hmlgrep: hmlgrep.hs Makefile
	ghc --make $< $(OPTS)

clean:
	rm -f hmlgrep
	rm -f hmlgrep.prof
	rm -f hmlgrep.o
	rm -f hmlgrep.hi
	rm -rf test-result
