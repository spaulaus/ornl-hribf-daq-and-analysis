#
FFLAGS= -O0 -fno-automatic
#
OBJS= scop.o\
      epics/fepics.o\
      epics/dslib.o
#
LIBS= lib/scoplib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/orphlib.a\
      /usr/acq/vme/vmelib.a
#
scop: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) -o scop $(OBJS) $(LIBS) -L/usr/X11R6/lib -lX11 -lpthread -lm
#
install: scop
	install -o hhirf -p scop scop.hep /usr/acq/wks
#
.f.o:
	f77 $(FFLAGS) -c $<
