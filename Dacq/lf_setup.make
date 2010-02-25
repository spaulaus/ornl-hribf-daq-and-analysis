#
FFLAGS= --o2 --sav
#
OBJS= lf_setup.o camlist.o
#
LIBS= ../Dvme/vmexxlib.a \
      /usr/hhirf/orphlib.a
#
lf_setup: $(OBJS) $(LIBS)
#
	lf95 $(FFLAGS) $(OBJS) $(LIBS)  -o lf_setup
.f.o:
	lf95 -c $(FFLAGS)  $<
