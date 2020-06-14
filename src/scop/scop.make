#
FFLAGS= -O0 -fno-automatic
#
OBJS= scop.o\
      /tera/varner/Acq-development/epics-scaler/fepics.o\
      /tera/varner/Acq-development/epics-scaler/dslib.o
#
LIBS= /usr/acq2/lib/scoplib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/orphlib.a\
      /usr/acq2/lib/vmelib.a
#
scop: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) -o scop $(OBJS) $(LIBS) -L/usr/X11R6/lib -lX11 -lpthread -lm
#
.f.o:
	f77 $(FFLAGS) -c $<
