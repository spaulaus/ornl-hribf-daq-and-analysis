BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

LIBS=$(LIBDIR)/acqlib.a $(LIBDIR)/ipclib.a
INCS=$(INCDIR)/orphmsg.h $(INCDIR)/ipcdefs.h $(INCDIR)acqlib.h

CFLAGS= -O2 -I$(INCDIR) -Wall

logger: logger.c $(LIBS) $(INC)
	cc $(CFLAGS) -o logger logger.c $(LIBS)

install: logger
	install logger $(INSTALLDIR)
