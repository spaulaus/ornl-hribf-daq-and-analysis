LIBS= acqlib.a ../Dshm/ipclib.a
INCS= ../Dshm/ipcdefs.h acqshm.h acqlib.h
CFLAGS =

shmdidle: shmdidle.o  $(OBJECTS) $(LIBS)
	cc $(CFLAGS) -o shmdidle shmdidle.o $(OBJECTS) $(LIBS)

