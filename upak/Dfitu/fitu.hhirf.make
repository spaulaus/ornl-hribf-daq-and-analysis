#
FFLAGS= -O -fno-automatic
#
OBJS= /usr/hhirf/fitu.o\
      ./userfox.o
#
LIBS= /usr/hhirf/fitulib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/milibb.a\
      /usr/hhirf/orphlib.a
#
fitu: $(OBJS) $(LIBS)
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o fitu -L/usr/X11R6/lib -lX11
#
.f.o:
	f77 $(FFLAGS) -c $<
