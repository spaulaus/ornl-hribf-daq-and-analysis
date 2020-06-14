#
FFLAGS= -O0 -fno-automatic
#
OBJS= paccmsg.o  
#
LIBS= /usr/hhirf/orphlib.a
#
paccmsg: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o paccmsg
#
.f.o:
	f77 $(FFLAGS) -c $<
