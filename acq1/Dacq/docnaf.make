LIBS=vmelib.a /usr/hhirf/orphlib.a
INCS=docnaf.h pkt_io.h ../Dlan/vmedII.h
INCSDIR=
CFLAGS=
OBJS =

docnaf: docnaf.o  $(LIBS) $(INCS)
	$(CC) $(CFLAGS) -o docnaf docnaf.o $(OBJS) $(LIBS)

