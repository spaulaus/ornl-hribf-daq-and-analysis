LIBS=
INCS=
CFLAGS = -O2
OBJECTS = 

vmeterm1: vmeterm1.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmeterm1 vmeterm1.c  $(OBJECTS) $(LIBS)

