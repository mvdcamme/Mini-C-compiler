#!/bin/bash

FIXTURES="./input/fixtures"
OUTPUT_FOLDER="./output/fixtures"

# Set the proper escape character for printing colour in the console
if [[ "$OSTYPE" == "linux-gnu" ]]; then
	COLOR_ESCAPE_CHAR=e
elif [[ "$OSTYPE" == "darwin"* ]]; then
	COLOR_ESCAPE_CHAR=x1B
fi

clear && printf '\e[3J'

for INPUT in $FIXTURES/test*_in.c
do
	echo Compiling $INPUT
	APPLICATION_NAME=${INPUT%"_in.c"}
	OUTPUT="$APPLICATION_NAME.asm"
	OUTPUT_BASENAME="$(basename -- $OUTPUT)"
	sh ./run.sh $INPUT $OUTPUT --silent
	cp $OUTPUT $OUTPUT_FOLDER/$OUTPUT_BASENAME
done
