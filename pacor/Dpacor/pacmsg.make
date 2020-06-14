#
FFLAGS= -O0 -fno-automatic
#
OBJS= pacmsg.o  
#
LIBS= /usr/hhirf/orphlib.a
#
pacmsg: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o pacmsg
#
.f.o:
	f77 $(FFLAGS) -c $<
