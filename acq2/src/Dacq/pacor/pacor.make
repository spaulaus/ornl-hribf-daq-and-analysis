BASE=../../
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib
DIRWTM=/usr/hhirf

LIBS= ./lib/pacorlib.a\
      $(DIRWTM)/Dorphlib/orphlib.a\
      $(DIRWTM)/Dmilibb/milibb.a\
      $(LIBDIR)/vmelib.a
#
FFLAGS= -g -O0 -fno-automatic
#
OBJS= pacor.o
#
LIBS= ./lib/pacorlib.a\
      /usr/hhirf/orphlib.a\
      /usr/hhirf/milibb.a\
      $(LIBDIR)/vmelib.a
#
pacor: $(OBJS) $(LIBS)
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o pacor
#
install: pacor
	install pacor $(INSTALLDIR)

.f.o:
	f77 $(FFLAGS) -c  $<
