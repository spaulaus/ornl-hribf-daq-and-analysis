FFLAGS= -g -O2 -fno-automatic
CFLAGS= -g -O2
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
      doecounts.o\
      readum1.o
#
LIBS= /tera/varner/hribf/hhirf/lemorlib.a\
      /usr/hhirf/scatlibr.a\
      /usr/acq/vme/vmelib.a\
      /usr/acq/wks/acqlib.a\
      /usr/acq/wks/ipclib.a\
      /usr/hhirf/milibb.a\
      /usr/hhirf/orphlib.a\
      /usr/acq/vme/vmexxlib.a
#
#
tape: $(OBJS) $(LIBS)
	f77 $(FFLAGS) $(OBJS) $(LIBS) $(LDF) -o tape

clean:
	rm *.o tape

.c.o:
	gcc $(CFLAGS) -c  $<

.f.o:
	f77 $(FFLAGS) -c  $<

