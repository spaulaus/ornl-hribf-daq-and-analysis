#
FFLAGS= -O0 -fno-automatic
#
OBJS= numass03.o\
      massex03.o\
      getaz.o
#
LIBS= /usr/hhirf/orphlib.a
#
numass03: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o numass03
.f.o:
	f77 $(FFLAGS) -c $<
