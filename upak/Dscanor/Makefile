#This file builds the scanor executable with example subs.
FFLAGS= $(OPT) $(FARGS)
#
#
SCANORLIBS= ../Dscanorlib/scanorlib.a\
      $(INSTALLDIR)/orphlib.a\
      $(INSTALLDIR)/ipclib.a

SCANOFLIBS= ../Dscanorlib/scanorlib.a\
      $(INSTALLDIR)/orphlib.a

scanor: scanor.o scanusubs.o $(SCANORLIBS)
	$(F77) $(FFLAGS) scanor.o scanusubs.o $(SCANORLIBS) $(FLIBS) -o scanor

scanof: scanof.o scanusubs.o $(SCANOFLIBS)
	$(F77) $(FFLAGS) scanof.o scanusubs.o $(SCANOFLIBS) $(FLIBS) -o scanof

.f.o:
	$(F77) $(FFLAGS) -c $<

install: scanor scanof
	install -d $(INSTALLDIR)
	install -p scanor.o $(INSTALLDIR)
	rm -f *.exe  scanor
	install -p scanor.gfortran.make $(INSTALLDIR)
	install -p scanof.o $(INSTALLDIR)
	rm -f *.exe  scanof
	install -p scanof.gfortran.make $(INSTALLDIR)
	install -p scanor.hep $(INSTALLDIR)
