LIBS=../Dacq/vmelib.a
INCS=../Dacq/pkt_io.h ../Dlan/orph_pf.h vme_sys.h mem_mgr.h
CFLAGS = -O2
OBJECTS = 

run: run.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o run run.c $(OBJECTS) $(LIBS)

