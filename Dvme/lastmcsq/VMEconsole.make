LIBS=
INCS=
CFLAGS = -O2
OBJECTS = 

VMEconsole: VMEconsole.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o VMEconsole VMEconsole.c  $(OBJECTS) $(LIBS)

