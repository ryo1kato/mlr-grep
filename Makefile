ifeq ($(MAKECMDGOALS),prof)
    OPTS = -rtsopts -prof -auto-all
else
ifeq ($(MAKECMDGOALS),static)
    OPTS = -static -optl-static -optl-pthread
else
    OPTS = -O2
endif
endif

all: hmlgrep
prof static: all

perftest: test-result/test.data
	./perftest.sh

test-result/test.data:
	mkdir -p test-result
	./test/gentestlog.sh

hmlgrep: hmlgrep.hs Makefile
	ghc --make $< $(OPTS)

clean:
	rm -f hmlgrep
	rm -f hmlgrep.prof
	rm -f hmlgrep.o
	rm -f hmlgrep.hi
	rm -rf test-result
