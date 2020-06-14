LIBS=../Dlan/orph_pf.a
INCS=../Dlan/orph_pf.h
CFLAGS = -O2
OBJECTS = 

extract: extract.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o extract extract.c  $(OBJECTS) $(LIBS)

