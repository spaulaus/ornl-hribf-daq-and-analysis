LIBS=
INCS=
CFLAGS = -O2
OBJECTS = 

vmeterm: vmeterm.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmeterm vmeterm.c  $(OBJECTS) $(LIBS)

