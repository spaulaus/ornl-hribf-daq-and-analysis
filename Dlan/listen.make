INCS = orph_pf.h

CFLAGS = -O2

listen: listen.c $(INCS)
	$(CC) $(CFLAGS) -o listen listen.c

