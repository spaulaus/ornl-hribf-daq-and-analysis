BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

LIBS=$(LIBDIR)/vmelib.a /usr/hhirf/orphlib.a
INCS=docnaf.h $(INCDIR)/pkt_io_udp.h
CFLAGS= -O -I$(INCDIR) -Wall
docnaf: docnaf.c  $(LIBS) $(INCS)
	$(CC) $(CFLAGS) -o docnaf docnaf.c $(OBJS) $(LIBS)

install: docnaf
	install -p docnaf $(INSTALLDIR)
