CFLAGS= -O2
#
#
LIBS= vmelib.a /usr/hhirf/orphlib.a
cnaf: cnaf.c $(LIBS) $(INCS)
	cc $(CFLAGS) cnaf.c $(LIBS) -o cnaf
