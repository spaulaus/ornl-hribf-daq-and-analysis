BASE=..
INSTALLDIR=$(BASE)/bin
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib

LIBS= $(LIBDIR)/vmelib.a $(LIBDIR)/acqlib.a $(LIBDIR)/ipclib.a
INCS= $(INCDIR)/ipcdefs.h  $(INCDIR)/acqlib.h 

CFLAGS = -O2
OBJECTS = 

ipcmon: ipcmon.c  $(OBJECTS) $(INCS) $(LIBS)
	$(CC) $(CFLAGS) -o ipcmon ipcmon.c  $(OBJECTS) $(LIBS)

install: ipcmon
	install ipcmon $(INSTALLDIR)
