LIBS=../Dacq/vmelib.a /usr/hhirf/orphlib.a
INCS=../Dacq/pkt_io.h ../Dlan/orph_pf.h ../Dlan/vmedII.h mem_mgr.h
CFLAGS = -O2
OBJECTS = 

vmeboot: vmeboot.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmeboot vmeboot.c $(OBJECTS) $(LIBS)

