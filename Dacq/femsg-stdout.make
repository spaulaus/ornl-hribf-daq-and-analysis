BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

LIBS=$(LIBDIR)/vmelib.a 
INCS=$(INCDIR)/spkt_io_udp.h  $(INCDIR)/orph_udp.h $(INCDIR)/orphmsg.h
CFLAGS = -g -I$(INCDIR)
OBJECTS = 

femsg-stdout: femsg-stdout.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o femsg-stdout femsg-stdout.c  $(OBJECTS) $(LIBS)

