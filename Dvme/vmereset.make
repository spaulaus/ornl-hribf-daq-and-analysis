LIBS=../Dacq/vmelib.a
INCS=../Dacq/pkt_io.h ../Dlan/orph_pf.h ../Dlan/vmedII.h
CFLAGS = -O2
OBJECTS = 

vmereset: vmereset.c $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmereset vmereset.c $(OBJECTS) $(LIBS)

