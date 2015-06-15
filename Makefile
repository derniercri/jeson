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

init_travis:
	mkdir -p ebin


#Run the Test
test:init_travis all ${TEST} run_test
	erl -pa ebin -noshell -s run_test run -a ${TEST}

local_test: all ${TEST}
	./test.sh ${TEST}

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
