LIBS=acqlib.a ../Dshm/ipclib.a
INCS= ../Dshm/ipcdefs.h acqshm.h acqlib.h
CFLAGS= -O2

shmspy: shmspy.c  $(OBJECTS) $(LIBS)
	cc $(CLFAGS) -o shmspy shmspy.c $(OBJECTS) $(LIBS)

