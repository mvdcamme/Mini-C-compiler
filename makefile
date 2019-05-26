SOURCE_FOLDER = src
OUTPUT_FOLDER = output
OUTPUT_FILE = $(OUTPUT_FOLDER)/Main
INPUT_FILE = $(SOURCE_FOLDER)/test.c

all:
	clear && printf '\e[3J'
	echo `pwd`	
	happy $(SOURCE_FOLDER)/Main.y -o $(SOURCE_FOLDER)/Main.hs
	ghc $(SOURCE_FOLDER)/Main.hs -i$(SOURCE_FOLDER) -o $(OUTPUT_FILE)
	./$(OUTPUT_FILE) $(INPUT_FILE)