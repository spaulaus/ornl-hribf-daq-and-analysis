LIBS=
INCS=
CFLAGS = -O2
OBJECTS = 

logscan: logscan.c  $(OBJECTS) $(INCS)
	$(CC) $(CFLAGS) -o logscan logscan.c  $(OBJECTS) $(LIBS)

