FC = gfortran
FFLAGS = -O2

SRC = day08.f90 main.f90 tests.f90
LIB = day08.o
MOD = day08.mod
EXE = main tests

all: $(EXE)

$(LIB): day08.f90
	$(FC) $(FFLAGS) -c $<

main: $(LIB) main.f90
	$(FC) $(FFLAGS) -o $@ $^

tests: $(LIB) tests.f90
	$(FC) $(FFLAGS) -o $@ $^

clean:
	rm -f $(OBJ) $(EXE) $(LIB) $(MOD)
