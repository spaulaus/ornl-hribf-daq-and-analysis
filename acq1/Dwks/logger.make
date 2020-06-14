CFLAGS= -O2

LIBS=  ../Dshm/ipclib.a acqlib.a

INCS=  orphmsg.h ../Dshm/ipcdefs.h acqlib.h

logger: logger.c $(LIBS) $(INC)
	cc $(CFLAGS) -o logger logger.c $(LIBS)
