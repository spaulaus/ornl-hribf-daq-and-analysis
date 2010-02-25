CFLAGS= -O2

INCS = orpas_data.h ../Dlan/orph_pf.h orphmsg.h acqshm.h acqlib.h

LIBS=acqlib.a ../Dshm/ipclib.a ../Dacq/vmelib.a ../Dlan/orph_pf.a

OBJECTS = pktproc.o

pftoipc: pftoipc.c $(INCS) $(OBJECTS) $(LIBS)
	cc $(CFLAGS) -o pftoipc pftoipc.c $(OBJECTS) $(LIBS)

pktproc.o: $(INCS)
