LIBS=../Dacq/vmelib.a
INCS=../Dlan/vmedII.h  ../Dlan/orph_pf.h ../Dacq/pkt_io.h
CFLAGS = -O2
OBJECTS = 

kt: kt.c  $(OBJECTS) $(INCS) $(LIBS) $(LIBS)
	$(CC) $(CFLAGS) -o kt kt.c $(OBJECTS) $(LIBS)

