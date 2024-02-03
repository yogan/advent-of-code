FPC = fpc -Sa

MAIN_SOURCE = aoc.pas
MAIN_OUTPUT = aoc

# TEST_SOURCE = aoc_test.pas
# TEST_OUTPUT = aoc_test

all: $(MAIN_OUTPUT)# $(TEST_OUTPUT)

$(MAIN_OUTPUT): $(MAIN_SOURCE)
	$(FPC) $<

# $(TEST_OUTPUT): $(TEST_SOURCE)
# 	$(FPC) $<

# test: $(TEST_OUTPUT)
# 	./$(TEST_OUTPUT)

clean:
	# rm -f $(MAIN_OUTPUT) $(TEST_OUTPUT) *.o *.ppu
	rm -f $(MAIN_OUTPUT) *.o *.ppu

