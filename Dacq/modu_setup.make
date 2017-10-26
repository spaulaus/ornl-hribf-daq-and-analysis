BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

LIBS= $(LIBDIR)/vmelib.a $(LIBDIR)/vmexxlib.a /usr/hhirf/orphlib.a 
INCS= 
#
FFLAGS= --o2 --sav
#
OBJS= modu_setup.o
#
modu_setup: $(OBJS) $(LIBS)
	lf95 $(FFLAGS) $(OBJS) $(LIBS)  -o modu_setup

install: modu_setup
	install modu_setup $(INSTALLDIR)

.f.o:
	lf95 -c $(FFLAGS)  $<
