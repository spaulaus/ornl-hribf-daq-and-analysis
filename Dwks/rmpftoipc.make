CFLAGS= -g -O2

INCS = orpas_data.h ../Dlan/orph_pf.h orphmsg.h acqshm.h acqlib.h

LIBS=acqlib.a ../Dshm/ipclib.a ../Dacq/vmelib.a

OBJECTS = rmpktproc.o

rmpftoipc: rmpftoipc.c $(INCS) $(OBJECTS) $(LIBS)
	cc $(CFLAGS) -o rmpftoipc rmpftoipc.c $(OBJECTS) $(LIBS)

rmpktproc.o: $(INCS)
