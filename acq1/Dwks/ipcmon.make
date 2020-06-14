LIBS= acqlib.a ../Dshm/ipclib.a
INCS= ../Dshm/ipcdefs.h acqlib.h
CFLAGS = -O2
OBJECTS = 

ipcmon: ipcmon.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o ipcmon ipcmon.c  $(OBJECTS) $(LIBS)

