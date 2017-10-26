BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

CFLAGS= -O2
#
#
LIBS = /usr/hhirf/orphlib.a $(LIBDIR)/vmelib.a

cnaf: cnaf.c  $(INCS)
	cc $(CFLAGS) cnaf.c $(LIBS) -o cnaf

install: cnaf
	install cnaf $(INSTALLDIR)
