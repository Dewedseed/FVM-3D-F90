
# compile flags
FTN     = gfortran
FFLAGS  = -g -fbacktrace -O0
CPPFLAG = -cpp -P
CPPOPT  = -DUNIT_TEST
CPP     = $(CPPFLAG) $(CPPOPT)
LFLAG        = -march=native -ffpe-trap=zero
EXEC    = test

.f90.o:
			$(FTN) $(CPP) $(FFLAGS) -c $*.f90

FSRC_MAIN = \
			test_obj_file.f90 \
			program_test.f90

FOBJ_MAIN = $(FSRC_MAIN:.f90=.o)

HEAD_MAIN =

$(FOBJ_MAIN): $(HEAD_MAIN)
			$(FTN) $(CPP) $(FFLAGS) -c $*.f90

$(EXEC): $(FOBJ_MAIN)
			$(FTN) $(CPP) $(LFLAG) -o $(EXEC) *.o $(LLIBS)

clean:
			rm -f *.o
			rm -f *.mod
			rm -f $(EXEC)
