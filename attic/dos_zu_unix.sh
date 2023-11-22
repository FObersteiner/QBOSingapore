#!/bin/sh

# Umwandlung von DOS-Textdateien in UNIX-Textdateien
# by Th. Bergmann

if ([ 1 -ne $# ])
then
	echo "Parameterangabe stimmt nicht.\n"
	echo "usage: $0 <Datei>"
else
	cp $1 /tmp/$1 && rm $1 && dos2unix /tmp/$1 ./$1
fi

