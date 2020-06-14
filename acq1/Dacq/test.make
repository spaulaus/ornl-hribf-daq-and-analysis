#
FFLAGS= --o2 --sav
#
OBJS= test.o
#
LIBS= vmelib.a \
      ../Dvme/vmexxlib.a \
      /usr/hhirf/orphlib.a
#
test: $(OBJS) $(LIBS)
#
	gcc $(FFLAGS) $(OBJS) $(LIBS)  -o test
