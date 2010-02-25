CFLAGS= -O2

LIBS= ../Dacq/vmelib.a acqlib.a ../Dshm/ipclib.a

INCS= ../Dacq/pkt_io.h ../Dlan/vmedII.h ../Dlan/orph_pf.h orphmsg.h \
      ../Dshm/ipcdefs.h acqlib.h

femsg: femsg.c $(OBJECTS) $(LIBS) $(INCS)
	$(CC) $(CFLAGS) -o femsg femsg.c $(OBJECTS) $(LIBS)
