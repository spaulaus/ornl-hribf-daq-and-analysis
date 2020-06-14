CFLAGS= -O2
#
#
LIBS= vmelib.a
xfifo: xfifo.c $(LIBS) $(INCS)
	cc $(CFLAGS) xfifo.c $(LIBS) -o xfifo
