BASE=..
LIBDIR= $(BASE)/lib
INCDIR=$(BASE)/include
INCS = $(INCDIR)/orpas_data.h $(INCDIR)/orph_udp.h $(INCDIR)/orphmsg.h $(INCDIR)/acqshm.h $(INCDIR)/acqlib.h

LIBS=$(LIBDIR)/acqlib.a $(LIBDIR)/ipclib.a $(LIBDIR)/vmelib.a

CFLAGS= -g -O0 -I$(INCDIR)

OBJECTS = pktproc.o

udptoipc: udptoipc.o $(INCS) $(OBJECTS) $(LIBS)
	cc $(CFLAGS) -o udptoipc udptoipc.c $(OBJECTS) $(LIBS)

pktproc.o: $(INCS)

install: udptoipc
	install udptoipc $(BASE)/bin

clean:
	rm *.o udptoipc
