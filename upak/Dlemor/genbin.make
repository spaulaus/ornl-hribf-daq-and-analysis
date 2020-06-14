#
FFLAGS= -O0 -fno-automatic
#
OBJS= genbin.o  
#
LIBS= /usr/hhirf/orphlib.a
#
genbin: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o genbin
.f.o:
	f77 $(FFLAGS) -c $<
