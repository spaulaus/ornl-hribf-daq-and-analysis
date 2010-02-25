CFLAGS= -O2
#
#
LIBS= vmelib.a
fifo: fifo.c $(LIBS) $(INCS)
	cc $(CFLAGS) fifo.c $(LIBS) -o fifo
