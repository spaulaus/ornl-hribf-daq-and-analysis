#Makefile
# ipctotcp sender and test program, buffertcpclient
#
LIBS=$(libdir)/acqlib.a $(libdir)/ipclib.a $(libdir)/vmelib.a

CFLAGS= $(OPTS) $(CARGS)

all: ipctotcp buffertcpclient

ipctotcp: ipctotcp.o 
	$(CC) $(CFLAGS) -o ipctotcp ipctotcp.c $(LIBS)

buffertcpclient: buffertcpclient.o
	$(CC) $(CFLAGS) -o buffertcpclient buffertcpclient.c $(LIB)

install: ipctotcp buffertcpclient
	install ipctotcp $(bindir)
	install buffertcpclient $(bindir)

clean:
	rm -f *.o ipctotcp buffertcpclient
