LIBS= /usr/hhirf/orphlib.a
INCS=
CFLAGS = -O2
OBJECTS = 

ALPHAterm: ALPHAterm.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o ALPHAterm ALPHAterm.c  $(OBJECTS) $(LIBS)

