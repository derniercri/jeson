#!/bin/bash

test_module(){
    echo "=================="
    echo "Testing modules : $1"
    echo "=================="
    erl -pa ebin -noshell -s $1 test -s init stop
}

test_list(){
    for i in $1
    do
	if ! test -e ebin/$i.beam
	then
	    echo "error : file ebin/$i.beam don't exist"
	else
	    test_module $i
	fi
    done
}

test_list $*


