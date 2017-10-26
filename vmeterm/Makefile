ROOT=..
LIBS=
INCS=$(ROOT)/include
CFLAGS = -O2
OBJECTS = 

vmeterm: vmeterm.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmeterm vmeterm.c  $(OBJECTS) $(LIBS)

install:
	install vmeterm $(ROOT)/bin

clean:
	rm vmeterm
