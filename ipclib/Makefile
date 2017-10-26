CFLAGS = -O2 -Wall
BASE=..

ipclib.a: ipclib.a(create_shm.o)\
 ipclib.a(attach_shm.o)\
 ipclib.a(map_shm.o)\
 ipclib.a(create_msg.o)\
 ipclib.a(attach_msg.o)\
 ipclib.a(remove_msg.o)\
 ipclib.a(create_sem.o)\
 ipclib.a(attach_sem.o)\
 ipclib.a(remove_sem.o)
	ranlib ipclib.a
.c.o:
	cc -c $(CFLAGS) $<
#	ar rv ipclib.a $*.o
#	rm -f $*.o
install: ipclib.a
	install ipclib.a $(BASE)/lib
	install ipcdefs.h $(BASE)/include
	install ipckeys.h $(BASE)/include

clean:
	rm ipclib.a *.o
