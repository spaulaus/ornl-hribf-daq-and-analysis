BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

LIBS= $(LIBDIR)/vmelib.a
INCS= $(INCDIR)/pkt_io_udp.h  $(INCDIR)/orph_udp.h $(INCDIR)/acq_ctl.h
CFLAGS = -g -I$(INCDIR) -Wall
OBJECTS =

acq_run: acq_run.c  $(OBJECTS) $(INCS) $(LIBS)
	gcc $(CFLAGS) -o acq_run acq_run.c $(OBJECTS) $(LIBS)

install: acq_run
	install acq_run $(INSTALLDIR)
	install startvme $(INSTALLDIR)
	install stopvme $(INSTALLDIR)
	install statvme $(INSTALLDIR)
	install initvme $(INSTALLDIR)
	install pacfile $(INSTALLDIR)

