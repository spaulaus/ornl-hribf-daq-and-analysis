#
FFLAGS= -O0 -fno-automatic
#
OBJS= numass95.o
#
LIBS= /usr/hhirf/orphlib.a
#
numass95: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o numass95
.f.o:
	f77 $(FFLAGS) -c $<
