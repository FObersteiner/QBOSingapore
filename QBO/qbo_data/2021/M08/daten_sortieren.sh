#!/bin/bash


FILE=/home/seiling/Dokumente/sing20210825.00
FILEOUT=/home/seiling/Dokumente/sing20210825.00

#cat $FILE | cut -c 1-54  #Letzten Zeichen wurden rausgeschnitten
#cat $FILE | fold -s -w 54 $FILE  #Zeilenumbruch mit vielen Leerzeichen dahinter


for i in sing* ; do fold -w 54 $i > tmpfile; mv tmpfile $i; done


