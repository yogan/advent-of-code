FC = gfortran
FFLAGS = -O2

SRC = aoc.f90 main.f90 tests.f90
LIB = aoc.o
MOD = aoc.mod
EXE = main tests

all: $(EXE)

$(LIB): aoc.f90
	$(FC) $(FFLAGS) -c $<

main: $(LIB) main.f90
	$(FC) $(FFLAGS) -o $@ $^

tests: $(LIB) tests.f90
	$(FC) $(FFLAGS) -o $@ $^

clean:
	rm -f $(OBJ) $(EXE) $(LIB) $(MOD)
