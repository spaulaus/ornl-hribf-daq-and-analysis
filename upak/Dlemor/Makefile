#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= lemor.o
#
LIBS= ../Dlemorlib/lemorlib.a\
      ../Dmilibb/milibb.a\
      ../Dorphlib/orphlib.a
#
lemor: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS)  -o lemor
.f.o:
	$(F77) $(FFLAGS) -c  $<
install: lemor
	install -d $(INSTALLDIR)
	install -p lemor $(INSTALLDIR)
	rm -f *.exe  lemor
	install -p lemor.hep $(INSTALLDIR)
	install -p lemor.hhirf.make $(INSTALLDIR)/lemor.make
