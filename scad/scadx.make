#
FFLAGS= -O0 -fno-automatic
#
OBJS= scad.o\
      cmpscad.o\
      dometer.o\
      glimset.o\
      glabndx.o\
      gnitter.o\
      scadnit.o\
      statman.o
#
LIBS= /tera/milner/DDlinux/Dscadlib/scadlib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/orphlib.a\
      /usr/hhirf/vmelib.a
#
scadx: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) -o scadx $(OBJS) $(LIBS) -L/usr/X11R6/lib -lX11 -lpthread
.f.o:
	f77 $(FFLAGS) -c $<
