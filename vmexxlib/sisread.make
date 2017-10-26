FORTRAN=/bin/gfortran
FFLAGS=-g
#
DIRA= /usr/hhirf/
DIRB=
DIRC= /usr/acq2/lib/
#
LIBS= $(DIRA)orphlib.a $(DIRB)vmexxlib.a $(DIRC)vmelib.a
sisread: sisread.f $(LIBS) $(INCS)
	$(FORTRAN) $(FFLAGS) sisread.f $(LIBS) -o sisread
