BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

LIBS=$(LIBDIR)/vmelib.a $(LIBDIR)/acqlib.a $(LIBDIR)/ipclib.a
INCS=$(INCDIR)/spkt_io_udp.h  $(INCDIR)/orph_udp.h $(INCDIR)/orphmsg.h
CFLAGS = -g -I$(INCDIR)
OBJECTS = 

femsg: femsg.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o femsg femsg.c  $(OBJECTS) $(LIBS)

install: femsg
	install femsg $(INSTALLDIR)
