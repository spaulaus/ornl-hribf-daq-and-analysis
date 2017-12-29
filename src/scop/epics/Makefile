CCFLAGS = -Wall -O
FFLAGS = $(OPTS) $(FARGS)

all: dslib.o fepics.o

client : client.o dslib.o
	cc -O -o client -lm \
	client.o dslib.o

test_fepics: fepics.o dataServer.h test_fepics.o dslib.o
	f77 -O -o test_fepics test_fepics.o fepics.o dslib.o -lm

test_fepics.o: test_fepics.f
	f77 ${FFLAGS} -o test_fepics.o -c test_fepics.f

fepics.o: fepics.c dataServer.h
	cc ${CCFLAGS} -o fepics.o -c fepics.c

client.o : client.c dataServer.h
	cc ${CCFLAGS} -o client.o -c client.c

dslib.o : dslib.c dataServerPriv.h dataServer.h dataServer.version
	cc ${CCFLAGS} -o dslib.o -c dslib.c

clean:
	rm -f *.o
