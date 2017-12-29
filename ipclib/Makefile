CFLAGS = $(OPT) $(CARGS)

$(includedir)/ipcdefs.h:
	install ipcdefs.h $(includedir)
	install ipckeys.h $(includedir)
	
ipclib.a: $(includedir)/ipcdefs.h \
 ipclib.a(create_shm.o)\
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

install: ipclib.a
	install ipclib.a $(libdir)

clean:
	rm -f ipclib.a *.o
