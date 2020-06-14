LIBS= ../Dacq/vmelib.a
INCS= ../Dacq/pkt_io.h ../Dlan/orph_pf.h devices.h mem_mgr.h
CFLAGS = -O2
OBJECTS =

vmehardware: vmehardware.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmehardware vmehardware.c  $(OBJECTS) $(LIBS)

