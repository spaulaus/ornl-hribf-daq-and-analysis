#
FFLAGS= -O0 -fno-automatic
#
OBJS= shomass.o\
      massexx.o\
      getaz.o  
#
LIBS= /usr/hhirf/orphlib.a
#
shomass: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o shomass
.f.o:
	f77 $(FFLAGS) -c $<
