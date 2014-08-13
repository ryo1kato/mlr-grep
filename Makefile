ifeq ($(MAKECMDGOALS),prof)
OPTS = -rtsopts -prof -auto-all
endif


all: hmlgrep
prof: all

hmlgrep: hmlgrep.hs Makefile
	ghc --make $< $(OPTS)

clean:
	rm -f hmlgrep
	rm -f hmlgrep.prof
	rm -f hmlgrep.o
	rm -f hmlgrep.hi
