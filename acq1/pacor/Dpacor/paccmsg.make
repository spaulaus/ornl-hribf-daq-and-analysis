#
FFLAGS= -O0 -fno-automatic
#
OBJS= paccmsg.o  
#
LIBS= /usr/hhirf/orphlib.a
#
paccmsg: $(OBJS) $(LIBS)
#
	gfortran $(FFLAGS) $(OBJS) $(LIBS) -o paccmsg
#
.f.o:
	gfortran $(FFLAGS) -c $<
