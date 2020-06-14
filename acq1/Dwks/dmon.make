LIBS=../Dacq/vmelib.a
INCS=../Dacq/pkt_io.h  ../Dlan/vmedII.h ../Dlan/orph_pf.h
CFLAGS = -O
OBJECTS = 

dmon: dmon.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o dmon dmon.c  $(OBJECTS) $(LIBS)

