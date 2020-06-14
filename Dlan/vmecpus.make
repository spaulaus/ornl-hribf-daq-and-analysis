CFLAGS = -O2

LIBS = 

OBJECTS = addr_reso.o

vmecpus: vmecpus.c  $(OBJECTS) $(INCS)
	$(CC) $(CFLAGS) -o vmecpus vmecpus.c  $(OBJECTS) $(LIBS)

