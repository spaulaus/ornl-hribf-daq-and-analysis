#
FFLAGS= -O0 -fno-automatic
#
OBJS= demiox.o 
#
LIBS= /usr/hhirf/orphlib.a
#
demiox: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o demiox
.f.o:
	f77 $(FFLAGS) -c $<
