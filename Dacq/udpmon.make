BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

LIBS= $(LIBDIR)/vmelib.a
INCS= $(INCDIR)/pkt_io_udp.h  $(INCDIR)/orph_udp.h 

CFLAGS = -O
OBJECTS = 

udpmon: udpmon.o  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o udpmon udpmon.c  $(OBJECTS) $(LIBS)

install: udpmon
	install udpmon $(INSTALLDIR)
