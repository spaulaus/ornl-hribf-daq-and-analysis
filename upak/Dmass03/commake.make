#
FFLAGS= -O0 -fno-automatic
#
OBJS=  commake.o
#
commake: $(OBJS)
#
	f77 $(FFLAGS) $(OBJS) -o commake
.f.o:
	f77 $(FFLAGS) -c $<
