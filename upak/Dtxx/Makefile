#
FFLAGS= $(OPT) $(FARGS)
CFLAGS= $(OPT) $(CARGS)
#
OBJS= txx.o 
#
LIBS=  ../Dtxxlib/txxlib.a\
       $(INSTALLDIR)/orphlib.a
#
txx: txx.o $(LIBS)
	$(F77) $(FFLAGS) txx.o $(LIBS) $(FLIBS) -o txx
#
txxps: txxps.o
	$(CC) $(CFLAGS) txxps.o $(LIBS) $(CLIBS) -o txxps
#
.c.o:
	$(CC) $(CFLAGS) -c $<
#
.f.o:
	$(F77) $(FFLAGS) -c $<
#
install: txx txxps
	install -d $(INSTALLDIR)
	install -p txx txxps $(INSTALLDIR)
	install -p txxps.ps ptxx dodoc viewdoc pdoc macview $(INSTALLDIR)
	rm -f *.exe  txx txxps
