LIBS=../Dacq/vmelib.a
INCS=../Dacq/pkt_io.h ../Dlan/orph_pf.h vme_sys.h mem_mgr.h
CFLAGS = -O2
OBJECTS = 

vmealloc: vmealloc.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmealloc vmealloc.c $(OBJECTS) $(LIBS)

