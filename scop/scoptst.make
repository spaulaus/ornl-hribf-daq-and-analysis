#
FFLAGS= -O0 -fno-automatic
#
OBJS= scop.o\
      tstruts.o\
      /tera/varner/Acq-development/epics-scaler/fepics.o\
      /tera/varner/Acq-development/epics-scaler/dslib.o
#
LIBS= /tera/milner/DDlinux/Dscoplib/scoplib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/orphlib.a\
      /usr/hhirf/vmelib.a
#
scoptst: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) -o scoptst $(OBJS) $(LIBS) -L/usr/X11R6/lib -lX11 -lpthread -lm
#
.f.o:
	f77 $(FFLAGS) -c $<
