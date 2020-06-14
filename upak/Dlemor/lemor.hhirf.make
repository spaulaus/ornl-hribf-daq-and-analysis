#
FFLAGS= -O0 -fno-automatic
#
OBJS= /usr/hhirf/lemor.o
#
LIBS= /usr/hhirf/lemorlib.a\
      /usr/hhirf/milibb.a\
      /usr/hhirf/orphlib.a
#
lemor: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS)  -o lemor
.f.o:
	f77 $(FFLAGS) -c  $<
