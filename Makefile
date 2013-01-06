SOURCE = $(wildcard *.hs)

all: $(SOURCE) build

run: all
	./Main

re: ready all

rerun: ready run

ready:
	rm -f Main Main.exe

build:
	ghc --make Main

clean: ready
	rm -f *.o *.hi
