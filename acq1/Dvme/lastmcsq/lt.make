LIBS= ../Dacq/vmelib.a
INCS=../Dlan/vmedII.h  ../Dacq/pkt_io.h mem_mgr.h
CFLAGS =  -O2
OBJECTS =

lt: lt.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o lt lt.c $(OBJECTS) $(LIBS)

