LIBS=../Dacq/vmelib.a
INCS=../Dlan/orph_pf.h ../Dacq/pkt_io.h ../Dlan/vmedII.h
CFLAGS = -O2
OBJECTS = 

datamon: datamon.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o datamon datamon.c  $(OBJECTS) $(LIBS)

