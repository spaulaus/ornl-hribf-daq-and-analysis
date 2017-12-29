#Makefile
# depends on Make one level up
#
FFLAGS= $(OPTS) $(FARGS)
CFLAGS= $(OPTS) $(CARGS)
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
LIBS= $(hhirfdir)/lemorlib.a\
      scatlib/scatlibr.a\
      $(libdir)/vmelib.a\
      $(libdir)/acqlib.a\
      $(libdir)/ipclib.a\
      $(libdir)/vmexxlib.a\
      $(hhirfdir)/milibb.a\
      $(hhirfdir)/orphlib.a
#
#
tape: $(OBJS) $(LIBS)
	$(FORT) $(FFLAGS) $(OBJS) $(LIBS) -o tape

scatlib/scatlibr.a:
	$(MAKE) -C scatlib 

install: tape
	install tape $(bindir)

clean:
	rm -f *.o tape
	$(MAKE) -C scatlib clean

.c.o:
	$(CC) $(CFLAGS) -c  $<

.f.o:
	$(FORT) $(FFLAGS) -c  $<

