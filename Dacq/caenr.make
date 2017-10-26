BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib
FORT=/usr/bin/gfortran

FFLAGS= -O2 -I$(INCDIR)
#
DIRA= /usr/hhirf/
#DIRB= /tera/mcsq/Drtems/Dvmexx/
#DIRC= /tera/mcsq/Drtems/Dacq/
#
LIBS= $(DIRA)/orphlib.a $(LIBDIR)/vmexxlib.a $(LIBDIR)/vmelib.a
caenr: caenr.f $(LIBS) $(INCS)
	$(FORT) $(FFLAGS) caenr.f $(LIBS) -o caenr

install: caenr
	install caenr $(INSTALLDIR)
