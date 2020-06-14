FFLAGS= -g -O2 -fno-automatic -fsecond-underscore
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
LIBS= /usr/hhirf/lemorlib.a\
      /usr/acq/wks/scatlibr.a\
      ../../Dacq/vmelib.a\
      ../acqlib.a\
      ../../Dshm/ipclib.a\
      /usr/hhirf/milibb.a\
      /usr/hhirf/orphlib.a\
      ../../Dvme/vmexxlib.a
#
#
tape: $(OBJS) $(LIBS)
	gfortran $(FFLAGS) $(OBJS) $(LIBS) $(LDF) -o tape

clean:
	rm *.o tape

.c.o:
	gcc $(CFLAGS) -c  $<

.f.o:
	gfortran $(FFLAGS) -c  $<

