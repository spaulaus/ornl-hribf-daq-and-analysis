#
FFLAGS= -O0 -fno-automatic
#
OBJS= getmass.o  
#
LIBS= /usr/hhirf/orphlib.a
#
getmass: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o getmass
.f.o:
	f77 $(FFLAGS) -c $<
