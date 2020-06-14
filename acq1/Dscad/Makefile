#
FFLAGS= -O0 -fno-automatic
#
OBJS= scad.o\
      scadmsg.o
#
LIBS= lib/scadlib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/orphlib.a\
      /usr/acq/vme/vmelib.a\
      /usr/acq/vme/vmexxlib.a
#
scad: $(OBJS) $(LIBS)
#
	gfortran $(FFLAGS) -o scad $(OBJS) $(LIBS) -L/usr/X11R6/lib -lX11 -lpthread
.f.o:
	gfortran $(FFLAGS) -c $<
