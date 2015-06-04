INCLUDE = include
EBIN = ebin
SRC = src
COMPILE = erlc -I $(INCLUDE) -pa $(EBIN) -o $(EBIN)
.PHONY: all clean


# Compile all modules
all: coers.beam

# General rules
%.beam: src/%.erl
	dialyzer $(<)
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
