LIBS=
INCS=
CFLAGS = -O2
OBJECTS = 

VMEterm: VMEterm.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o VMEterm VMEterm.c  $(OBJECTS) $(LIBS)

