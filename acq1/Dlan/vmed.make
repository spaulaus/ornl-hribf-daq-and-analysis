LIBS= orph_pf.a
INCS=orph_pf.h  vmed.h
CFLAGS = -O2 -DHHIRF
OBJECTS = 

vmed: vmed.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmed vmed.c $(OBJECTS) $(LIBS)

