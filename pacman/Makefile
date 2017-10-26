BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

LIBS= $(LIBDIR)/acqlib.a $(LIBDIR)/ipclib.a /usr/hhirf/orphlib.a
INCS= $(INCDIR)/ipcdefs.h $(INCDIR)/orphmsg.h $(INCDIR)/acqlib.h $(INCDIR)/acqshm.h $(INCDIR)/ipckeys.h
CFLAGS = -O2 -I$(INCDIR)
OBJECTS = 

pacman: pacman.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o pacman pacman.c  $(OBJECTS) $(LIBS)
install: pacman
	install pacman $(BASE)/bin
