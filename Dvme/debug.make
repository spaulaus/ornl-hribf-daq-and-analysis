LIBS=../Dacq/vmelib.a
INCS=../Dlan/vmedII.h ../Dacq/pkt_io.h
CFLAGS = -O2
OBJECTS = 

debug: debug.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o debug debug.c  $(OBJECTS) $(LIBS)

