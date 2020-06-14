#
FFLAGS= -O0 -fno-automatic
#
OBJS= demio.o 
#
LIBS= /usr/hhirf/orphlib.a
#
demio: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o demio
.f.o:
	f77 $(FFLAGS) -c $<
