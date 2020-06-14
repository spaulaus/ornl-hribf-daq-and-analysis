LIBS= acqlib.a ../Dshm/ipclib.a
INCS=
CFLAGS = -O2
OBJECTS = 

acqcleanup: acqcleanup.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o acqcleanup acqcleanup.c  $(OBJECTS) $(LIBS)

