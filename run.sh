#!/bin/sh

# Set the proper escape character for printing colour in the console
if [[ "$OSTYPE" == "linux-gnu" ]]; then
	COLOR_ESCAPE_CHAR=e
elif [[ "$OSTYPE" == "darwin"* ]]; then
	COLOR_ESCAPE_CHAR=x1B
fi

OUTPUT_FOLDER=output
COMPILER=$OUTPUT_FOLDER/build/mini-c-compiler/mini-c-compiler
INPUT_FILE=${1:-./src/test.c}
OUTPUT_FILE=${2:-.output/output.asm}
KEEP_SILENT=${3:-"--verbose"}
TO=./ASMBox/c_disk/EXERCISE

echo INPUT_FILE $INPUT_FILE
echo OUTPUT_FILE $OUTPUT_FILE

./$COMPILER $INPUT_FILE $OUTPUT_FILE $KEEP_SILENT
EXIT_CODE=$?
if [[ $EXIT_CODE -eq 0 ]]; then
	rm -f $TO/test.asm $TO/TEST.EXE $TO/TEST.OBJ
	cp $OUTPUT_FILE $TO/test.asm
	echo "\\$COLOR_ESCAPE_CHAR[42mCompiler finished successfully\\$COLOR_ESCAPE_CHAR[0m"
else
	echo "\\$COLOR_ESCAPE_CHAR[101mCompiler failed\\$COLOR_ESCAPE_CHAR[0m"
fi
