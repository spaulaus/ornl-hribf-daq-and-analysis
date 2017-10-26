DIRHHIRF=/usr/hhirf
BASE=..
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib
INSTALLDIR=$(BASE)/bin

FORT=/usr/bin/gfortran
FFLAGS= -O2 -fno-automatic -fsecond-underscore -fno-range-check -I$(INCDIR)
#FORT=/usr/bin/f77
#FFLAGS= -O2 -fno-automatic -fsecond-underscore -I$(INCDIR)
CFLAGS= -O2 -I$(INCDIR) -Wall
#
OBJS= tape.o\
      fortinput.o\
      lemonit.o\
      comset.o\
      ldfopen.o\
      cmpioman.o\
      cmpsetup.o\
      hedman.o\
      pacsavf.o\
      copyipc.o\
      readum1.o
#
LIBS= /usr/hhirf/lemorlib.a\
      scatlib/scatlibr.a\
      $(LIBDIR)/vmelib.a\
      $(LIBDIR)/acqlib.a\
      $(LIBDIR)/ipclib.a\
      /usr/hhirf/milibb.a\
      /usr/hhirf/orphlib.a
#
#
tape: $(OBJS) $(LIBS)
	$(FORT) $(FFLAGS) $(OBJS) $(LIBS) -o tape

install: tape
	install tape $(INSTALLDIR)

clean:
	rm *.o tape

.c.o:
	gcc $(CFLAGS) -c  $<

.f.o:
	$(FORT) $(FFLAGS) -c  $<

