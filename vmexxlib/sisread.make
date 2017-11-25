FORTRAN=/bin/gfortran
FFLAGS=-g
#
DIRA= /usr/hhirf/
DIRB=
DIRC= /usr/acq2/lib/
#
LIBS= $(DIRA)orphlib.a $(DIRC)vmexxlib.a $(DIRC)vmelib.a
sisread: sisread.f $(LIBS) $(INCS)
	$(FORTRAN) $(FFLAGS) sisread.f $(LIBS) -o sisread

install: sisread
	install sisread /usr/acq2/bin
