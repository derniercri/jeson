INCLUDE = include
EBIN = ebin
SRC = src
COMPILE = erlc -I $(INCLUDE) -pa $(EBIN) -o $(EBIN)
.PHONY: all clean
OBJ= coers.beam json_encoder.beam json_decoder.beam json.beam
TEST=json_decoder_test
# Compile all modules
all:${OBJ}

c_test:${TEST}

#Run the Test
test:all c_test
	./test.sh ${TEST}

# General rules
%.beam: src/%.erl
	dialyzer $(<)
	$(COMPILE) $(<)

#Test Rules
%_test: test/%_test.erl
	$(COMPILE) $(<)

# Run with the library
run: all
	erl -pa $(EBIN)


# Dialyzer initializer
init-dialyzer:
	dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl common_test eunit

# Clean binaries
clean:
	rm -rf $(EBIN)/*
