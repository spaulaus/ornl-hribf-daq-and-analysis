# Makefile
# udptoipc
#
LIBS=$(libdir)/acqlib.a $(libdir)/ipclib.a $(libdir)/vmelib.a

CFLAGS= $(OPTS) $(CARGS)

OBJECTS = pktproc.o

udptoipc: udptoipc.o pktproc.o 
	$(CC) $(CFLAGS) -o udptoipc udptoipc.c pktproc.o $(LIBS)

install: udptoipc
	install udptoipc $(bindir)

clean:
	rm -f *.o udptoipc
