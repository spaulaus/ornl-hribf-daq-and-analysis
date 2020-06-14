#
FFLAGS= -O2 -fnoautomatic
#
OBJS= modu_setup.o
#
LIBS= vmelib.a \
      ../Dvme/vmexxlib.a \
      /usr/hhirf/orphlib.a
#
modu_setup: $(OBJS) $(LIBS)
#
	gfortran $(FFLAGS) $(OBJS) $(LIBS)  -o modu_setup
.f.o:
	gfortran -c $(FFLAGS)  $<
