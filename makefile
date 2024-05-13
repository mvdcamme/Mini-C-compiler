SOURCE_FOLDER = src
OUTPUT_FOLDER = output
OUTPUT_FILE = $(OUTPUT_FOLDER)/build/mini-c-compiler/mini-c-compiler
HAPPY_OUTPUT_FILE = $(SOURCE_FOLDER)/Main.hs

all:
	clear && printf '\e[3J'
	rm -f $(HAPPY_OUTPUT_FILE) $(OUTPUT_FILE)
	# rm -f $(SOURCE_FOLDER)/*.hi $(SOURCE_FOLDER)/*.o
	happy $(SOURCE_FOLDER)/Main.y -o $(HAPPY_OUTPUT_FILE)
	cabal build --builddir=$(OUTPUT_FOLDER)
	