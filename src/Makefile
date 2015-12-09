IDIR =../include
ODIR=obj
LDIR =../lib
BINDIR = ../bin
TESTDIR = ../bin/tests
MODDIR=modules
# FOR LINUX
RM = rm
# FOR WINDOWS
#RM = del

# GNU COMPILER
FC = gfortran
FFLAGS = -Wall -ffree-line-length-0 -Wtabs -fdefault-real-8 -I$(IDIR) -J$(MODDIR)
# INTEL COMPILER
#FC = ifort
#FFLAGS = -warn all -O3

_TEST_CHARGED_DISC_OBJ = charged_disc.o 
TEST_CHARGED_DISC_OBJ = $(patsubst %,$(ODIR)/%,$(_TEST_CHARGED_DISC_OBJ))
_TEST_LINKED_CHARGED_DISC_OBJ = $(_TEST_CHARGED_DISC_OBJ) string_functions.o linked_charged_disc.o 
TEST_LINKED_CHARGED_DISC_OBJ = $(patsubst %,$(ODIR)/%,$(_TEST_LINKED_CHARGED_DISC_OBJ))
_TEST_DISC_PULSE_OBJ = $(_TEST_LINKED_CHARGED_DISC_OBJ) disc_pulse.o 
TEST_DISC_PULSE_OBJ = $(patsubst %,$(ODIR)/%,$(_TEST_DISC_PULSE_OBJ))


$(ODIR)/%.o: $(IDIR)/%.f90
	$(FC) -c -o $@ $< $(FFLAGS)

test_charged_disc: $(TEST_CHARGED_DISC_OBJ)
	$(FC) -o $(TESTDIR)/test_charged_disc $@.f90 $^ $(FFLAGS)
test_linked_charged_disc: $(TEST_LINKED_CHARGED_DISC_OBJ)
	$(FC) -o $(TESTDIR)/test_linked_charged_disc $@.f90 $^ $(FFLAGS)
test_disc_pulse: $(TEST_DISC_PULSE_OBJ)
	$(FC) -o $(TESTDIR)/test_disc_pulse $@.f90 $^ $(FFLAGS)

.PHONY: clean
clean:
	$(RM) $(MODDIR)/*.mod $(ODIR)/*.o $(BINDIR)/* $(TESTDIR)/*
