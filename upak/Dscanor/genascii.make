#
FFLAGS= -O0 -fno-automatic
#
OBJS= genascii.o  
#
LIBS= /usr/hhirf/orphlib.a
#
genascii: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o genascii
.f.o:
	f77 $(FFLAGS) -c $<
