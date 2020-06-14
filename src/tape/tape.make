DIRHHIRF=/usr/hhirf
BASE=..
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib
INSTALLDIR=$(BASE)/bin

FFLAGS= -O2 -fno-automatic -I$(INCDIR)
CFLAGS= -O2 -I$(INCDIR)
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
      /usr/hhirf/scatlibr.a\
      $(LIBDIR)/vmelib.a\
      $(LIBDIR)/acqlib.a\
      $(LIBDIR)/ipclib.a\
      /usr/hhirf/milibb.a\
      /usr/hhirf/orphlib.a
#
#
tape: $(OBJS) $(LIBS)
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o tape

install: tape
	install tape $(INSTALLDIR)

clean:
	rm *.o tape

.c.o:
	gcc $(CFLAGS) -c  $<

.f.o:
	f77 $(FFLAGS) -c  $<

