LIBS= acqlib.a ../Dshm/ipclib.a /usr/hhirf/orphlib.a
INCS= ../Dshm/ipcdefs.h orphmsg.h acqlib.h acqshm.h
CFLAGS = -O2
OBJECTS = 

pacman: pacman.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o pacman pacman.c  $(OBJECTS) $(LIBS)

