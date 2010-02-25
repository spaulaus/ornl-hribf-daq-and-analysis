#
FFLAGS= -g -O0 -fno-automatic
#
OBJS= pacor.o
#
LIBS= ../Dpacorlib/pacorlib.a\
      /usr/hhirf/orphlib.a\
      /usr/hhirf/milibb.a\
      ../../Dacq/vmelib.a
#
pacor: $(OBJS) $(LIBS)
#
	gfortran $(FFLAGS) $(OBJS) $(LIBS) -o pacor
.f.o:
	gfortran $(FFLAGS) -c  $<
