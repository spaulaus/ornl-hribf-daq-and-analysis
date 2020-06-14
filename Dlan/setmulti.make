CFLAGS = -O2

setmulti: setmulti.c  $(OBJECTS) $(INCS)
	$(CC) $(CFLAGS) -o setmulti setmulti.c  $(OBJECTS) $(LIBS)

