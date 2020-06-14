#
FFLAGS= -O0 -fno-automatic
#
OBJS=  massio.o
#
massio: $(OBJS)
#
	f77 $(FFLAGS) $(OBJS) -o massio
.f.o:
	f77 $(FFLAGS) -c $<
