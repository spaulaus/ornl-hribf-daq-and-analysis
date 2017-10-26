#
#FORT = /usr/bin/gfortran
#FFLAGS= -O0 -fno-automatic -fsecond-underscore -fno-range-check
FORT = /usr/bin/f77
FFLAGS= -O0 -fno-automatic -fsecond-underscore
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
      ../lib/vmelib.a
#
scop: $(OBJS) $(LIBS)
#
	$(FORT) $(FFLAGS) -o scop $(OBJS) $(LIBS) -L/usr/X11R6/lib -lX11 -lpthread -lm

install: scop
	install scop ../bin

clean:
	rm scop *.o
#
.f.o:
	$(FORT) $(FFLAGS) -c $<
