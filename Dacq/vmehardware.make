BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

LIBS= $(LIBDIR)/vmelib.a
INCS= $(INCDIR)/pkt_io_udp.h $(INCDIR)/mem_mgr.h
CFLAGS = -O2 -I$(INCDIR) -Wall
OBJECTS =

vmehardware: vmehardware.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmehardware vmehardware.c  $(OBJECTS) $(LIBS)

install: vmehardware
	install vmehardware $(INSTALLDIR)
