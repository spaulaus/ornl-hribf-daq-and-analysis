#
FORTRAN=/usr/bin/gfortran
FFLAGS= -O0 -fno-automatic -fsecond-underscore -fno-range-check
#FORTRAN=/usr/bin/f77
#FFLAGS= -O0 -fno-automatic -fsecond-underscore 
#
OBJS= scad.o\
      scadmsg.o
#
LIBS= ./lib/scadlib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/dammlib.a\
      /usr/hhirf/xglib.a\
      /usr/hhirf/orphlib.a\
      ../lib/vmelib.a\
      ../lib/vmexxlib.a
#
scad: $(OBJS) $(LIBS)
#
	$(FORTRAN) $(FFLAGS) -o scad $(OBJS) $(LIBS) -L/usr/X11R6/lib -lX11 -lpthread
.f.o:
	$(FORTRAN) $(FFLAGS) -c $<

install: scad
	install scad ../bin

clean:
	rm scad *.o
