LIBS=../Dacq/vmelib.a /usr/hhirf/orphlib.a
INCS=../Dacq/pkt_io.h ../Dlan/orph_pf.h mem_mgr.h
CFLAGS = -O2
OBJECTS = 

setclock: setclock.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o setclock setclock.c $(OBJECTS) $(LIBS)

