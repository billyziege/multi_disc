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
FFLAGS = -I$(IDIR) -J$(MODDIR)
#FFLAGS = -Wall -ffree-line-length-0 -Wtabs -fdefault-real-8 -I$(IDIR) -J$(MODDIR)
# INTEL COMPILER
#FC = ifort
#FFLAGS = -warn all -O3

_TEST_CHARGED_DISC_OBJ = charged_disc.o 
TEST_CHARGED_DISC_OBJ = $(patsubst %,$(ODIR)/%,$(_TEST_CHARGED_DISC_OBJ))
_TEST_LINKED_CHARGED_DISC_OBJ = $(_TEST_CHARGED_DISC_OBJ) string_functions.o linked_charged_disc.o 
TEST_LINKED_CHARGED_DISC_OBJ = $(patsubst %,$(ODIR)/%,$(_TEST_LINKED_CHARGED_DISC_OBJ))
_TEST_DISC_PULSE_OBJ = $(_TEST_LINKED_CHARGED_DISC_OBJ) disc_pulse.o 
TEST_DISC_PULSE_OBJ = $(patsubst %,$(ODIR)/%,$(_TEST_DISC_PULSE_OBJ))
_DERF_OBJ = amach.o dcsevl.o derm1.o derv1.o dinits.o erfin.o ermsg.o ierm1.o ierv1.o derf.o
DERF_OBJ =  $(patsubst %,$(ODIR)/%,$(_DERF_OBJ))
_TEST_INITIAL_DISC_PULSE_OBJ = $(_TEST_DISC_PULSE_OBJ) $(_DERF_OBJ) cdfnorlib.o distribution_parameters.o my_matrix.o distribution_fields.o initial_disc_pulse.o
TEST_INITIAL_DISC_PULSE_OBJ = $(patsubst %,$(ODIR)/%,$(_TEST_INITIAL_DISC_PULSE_OBJ))

$(ODIR)/%.o: $(IDIR)/%.f90
	$(FC) -c -o $@ $< $(FFLAGS)
$(ODIR)/%.o: $(IDIR)/%.f
	$(FC) -c -o $@ $< $(FFLAGS)

test_charged_disc: $(TEST_CHARGED_DISC_OBJ)
	$(FC) -o $(TESTDIR)/test_charged_disc $@.f90 $^ $(FFLAGS)
test_linked_charged_disc: $(TEST_LINKED_CHARGED_DISC_OBJ)
	$(FC) -o $(TESTDIR)/test_linked_charged_disc $@.f90 $^ $(FFLAGS)
test_disc_pulse: $(TEST_DISC_PULSE_OBJ)
	$(FC) -o $(TESTDIR)/test_disc_pulse $@.f90 $^ $(FFLAGS)
test_initial_disc_pulse: $(TEST_INITIAL_DISC_PULSE_OBJ)
	$(FC) -o $(TESTDIR)/test_disc_pulse $@.f90 $^ $(FFLAGS)

.PHONY: clean
clean:
	$(RM) $(MODDIR)/*.mod $(ODIR)/*.o $(BINDIR)/* $(TESTDIR)/*
