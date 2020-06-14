#
FFLAGS= -O0 -fno-automatic
#
OBJS= scad.o\
      scadmsg.o
#
LIBS= ../../../acq2/src/scad/lib/scadlib.a\
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
	f77 $(FFLAGS) -o scad $(OBJS) $(LIBS) -L/usr/X11R6/lib -lX11 -lpthread
.f.o:
	f77 $(FFLAGS) -c $<
