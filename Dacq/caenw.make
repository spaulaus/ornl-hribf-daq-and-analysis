HHIRFDIR=/usr/hhirf
BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib
FORT=/usr/bin/gfortran

FFLAGS= -O2 -I$(INCDIR)
#
#
LIBS= $(HHIRFDIR)/orphlib.a $(LIBDIR)/vmexxlib.a $(LIBDIR)/vmelib.a

caenw: caenw.f $(LIBS) $(INCS)
	$(FORT) $(FFLAGS) caenw.f $(LIBS) -o caenw

install: caenw
	install caenw $(INSTALLDIR)
