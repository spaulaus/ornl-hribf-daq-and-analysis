LIBS=
INCS=
CFLAGS = -O2
OBJECTS = 

vmeload: vmeload.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmeload vmeload.c  $(OBJECTS) $(LIBS)

