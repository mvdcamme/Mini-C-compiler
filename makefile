SOURCE_FOLDER = src
OUTPUT_FOLDER = output

all:
	echo `pwd`	
	happy $(SOURCE_FOLDER)/Main.y -o $(SOURCE_FOLDER)/Main.hs
	ghc $(SOURCE_FOLDER)/Main.hs -o $(OUTPUT_FOLDER)/Main