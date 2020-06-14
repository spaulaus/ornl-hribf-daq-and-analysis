#Makefile
#
CFLAGS = $(OPTS) $(CARGS)

vmereset: vmereset.c
	$(CC) $(CFLAGS) -o vmereset vmereset.c $(libdir)/vmelib.a
install: vmereset
	install -p vmereset $(bindir)
clean:
	rm -f *.o vmereset
