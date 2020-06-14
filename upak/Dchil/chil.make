#
FFLAGS= -O -fno-automatic
#
OBJS= chil.o
#
LIBS= /usr/hhirf/chillib.a\
      /usr/hhirf/orphlib.a
#
chil: $(OBJS) $(LIBS)
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o chil
#
.f.o:
	f77 $(FFLAGS) -c $<
