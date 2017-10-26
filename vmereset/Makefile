ROOT=../
INC=$(ROOT)/include
LIB=$(ROOT)/lib
INSTALLDIR=$(ROOT)/bin


LIBS=$(LIB)/vmelib.a
INCS=$(INC)/pkt_io_udp.h $(INC)/orph_udp.h
CFLAGS = -O2 -I $(INC)
OBJECTS = 

vmereset: vmereset.c $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmereset vmereset.c $(OBJECTS) $(LIBS)
install: vmereset
	install -p vmereset $(INSTALLDIR)
clean:
	rm *.o vmereset
