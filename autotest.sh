#!/bin/bash

C_DISK="/Users/mvdcamme/Projects/Language_Implementations/Mini-C-compiler/ASMBox/c_disk"
MOUNT_C_DISK="mount C $C_DISK"
Y_MOUNT="imgmount Y /Users/mvdcamme/Projects/Language_Implementations/Mini-C-compiler/ASMBox/dosbox/asm_dev_disk.iso -t iso"
PATHS="SET PATH=Y:\DOS;Y:\UTILS\WATCOM\BINW;Y:\UTILS\TASM\BIN;Y:\UTILS\MASM611\BIN;Y:\UTILS\DN;%PATH%"
WATCOM="SET WATCOM=Y:\UTILS\WATCOM"

EXERCISE="$C_DISK/EXERCISE"
FIXTURES="./input/fixtures"

# Set the proper escape character for printing colour in the console
if [[ "$OSTYPE" == "linux-gnu" ]]; then
	COLOR_ESCAPE_CHAR=e
elif [[ "$OSTYPE" == "darwin"* ]]; then
	COLOR_ESCAPE_CHAR=x1B
fi

clear && printf '\e[3J'

for INPUT in $FIXTURES/test*_in.c
do
	echo $INPUT
	APPLICATION_NAME=${INPUT%"_in.c"}
	OUTPUT="$EXERCISE/OUT.TXT"
	EXPECTED="$APPLICATION_NAME""_out.txt"

	echo "Testing application $INPUT"
	rm -f $OUTPUT
	sh ./run.sh $INPUT $OUTPUT --silent

	# /Applications/dosbox.app/Contents/MacOS/DOSBox -noautoexec -c "$C_DISK" -c "$Y_MOUNT" -c "$PATHS" -c "$WATCOM" -c "C:" -c "cd EXERCISE" -c "make" -c "test.exe"
	/Applications/dosbox.app/Contents/MacOS/DOSBox -noautoexec -c "$MOUNT_C_DISK" -c "$Y_MOUNT" -c "$PATHS" -c "$WATCOM" -c "C:" -c "cd EXERCISE" -c "make" -c "test.exe >> out.txt" &
	DOSBOX=$!
	sleep 15
	kill -9 $DOSBOX
	diff $OUTPUT $EXPECTED -w
	RESULT=$?

	if [[ $RESULT -eq 0 ]]; then
			echo -e "\\$COLOR_ESCAPE_CHAR[42mTest \"$APPLICATION_NAME\" passed\\$COLOR_ESCAPE_CHAR[0m"
		else
			echo -e "\\$COLOR_ESCAPE_CHAR[101mTest \"$APPLICATION_NAME\" failed\\$COLOR_ESCAPE_CHAR[0m"
			break
		fi
done
