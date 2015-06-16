INCLUDE = include
EBIN = ebin
SRC = src
COMPILE = erlc -I $(INCLUDE) -pa $(EBIN) -o $(EBIN)
.PHONY: all clean
OBJ= coers.beam json_encoder.beam json_decoder.beam json.beam
TEST=json_decoder_test json_encoder_test
DIALYZER=FALSE
# Compile all modules
all:${OBJ}

#Run the Test
test:all ${TEST} 
	/usr/lib/erl-test-runner/erl-test-runner ${EBIN} ${TEST}

# General rules
%.beam: src/%.erl
ifeq ($(DIALYZER), TRUE)
	dialyzer  $(<)
endif
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
