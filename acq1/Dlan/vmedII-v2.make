LIBS= orph_pf.a
INCS=orph_pf.h  vmedII.h
CFLAGS = -g -O2 -DHHIRF 
DEBUG = -DDEBUG
OBJECTS = 
CC=gcc

vmedII: vmedII-v2.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o vmedII vmedII-v2.c $(OBJECTS) $(LIBS)

vmedII-debug: vmedII-v2.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) $(DEBUG) -o vmedII-debug vmedII-v2.c $(OBJECTS) $(LIBS)

