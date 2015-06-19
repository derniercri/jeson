INCLUDE = include
EBIN = ebin
SRC = src
COMPILE = erlc -I $(INCLUDE) -pa $(EBIN) -o $(EBIN)
.PHONY: all clean
OBJ= coers.beam json_encoder.beam json_decoder.beam json.beam
TEST=json_decoder_test json_encoder_test coers_test
DIALYZER=FALSE
INSTALL_DIR=/usr/lib/jeson

# Compile all modules
all: init ${OBJ}

init:
	mkdir -p ./ebin

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

install:
	mkdir ${INSTALL_DIR}
	cp -R ./ebin ${INSTALL_DIR}/
	cp -R ./include ${INSTALL_DIR}/
	ln -s ${INSTALL_DIR} /usr/lib/erlang/lib/jeson

uninstall:
	rm  -Rf ${INSTALL_DIR}
	rm -f /usr/lib/erlang/lib/jeson
