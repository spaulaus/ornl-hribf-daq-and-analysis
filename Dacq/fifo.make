BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

CFLAGS= -O2 -I$(INCDIR)
#
#
LIBS= $(LIBDIR)/vmelib.a
fifo: fifo.o  
	cc $(CFLAGS) fifo.c $(LIBS) -o fifo

install: fifo
	install $^ $(INSTALLDIR)
