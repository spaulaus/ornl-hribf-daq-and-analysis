LIBS=../Dacq/vmelib.a
INCS=../Dacq/pkt_io.h ../Dlan/orph_pf.h ../Dlan/vmedII.h
CFLAGS = -O2
OBJECTS = 

srload: srload.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o srload srload.c $(OBJECTS) $(LIBS)

