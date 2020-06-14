#
#
CFLAGS = $(OPTS) $(CARGS)

vmeterm: vmeterm.c 
	$(CC) $(CFLAGS) -o vmeterm vmeterm.c 

install: vmeterm
	install vmeterm $(bindir)

clean:
	rm -f *.o vmeterm
