#Makefile
#pacman
#
LIBS= $(libdir)/acqlib.a $(libdir)/ipclib.a $(hhirfdir)/orphlib.a
CFLAGS = $(OPTS) $(CARGS)

pacman: pacman.c  $(INCS)
	$(CC) $(CFLAGS) -o pacman pacman.c  $(LIBS)
install: pacman
	install pacman $(bindir)

clean:
	rm -f pacman pacman.o
