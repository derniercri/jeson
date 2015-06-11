#!/bin/bash

for i in $*
do
    if ! test -e ebin/$i.beam
    then
	echo "error : file ebin/$i.beam doesn't exist"
    else
	echo "=================="
	echo "Testing module : $i"
	echo "=================="
	erl -pa ebin -noshell -s $i test -s init stop
    fi
done



