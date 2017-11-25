BASE=..
LIBDIR= $(BASE)/lib
INCDIR=$(BASE)/include
INCS = $(INCDIR)/orpas_data.h $(INCDIR)/orph_udp.h $(INCDIR)/orphmsg.h $(INCDIR)/acqshm.h $(INCDIR)/acqlib.h

LIBS=$(LIBDIR)/acqlib.a $(LIBDIR)/ipclib.a $(LIBDIR)/vmelib.a

CFLAGS= -g -O0 -I$(INCDIR)

OBJECTS =

all: ipctotcp buffertcpclient

ipctotcp: ipctotcp.o $(INCS) $(OBJECTS) $(LIBS)
	cc $(CFLAGS) -o ipctotcp ipctotcp.c $(OBJECTS) $(LIBS)

buffertcpclient: buffertcpclient.o
	cc $(CFLAGS) -o buffertcpclient buffertcpclient.c $(OBJECTS) $(LIB)

install: ipctotcp buffertcpclient
	install ipctotcp $(BASE)/bin
	install buffertcpclient $(BASE)/bin

clean:
	rm *.o ipctotcp buffertcpclient
