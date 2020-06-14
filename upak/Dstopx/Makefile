#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= stopx.o
#
LIBS= ../Dstopxlib/stopxlib.a\
      $(INSTALLDIR)/milibb.a\
      $(INSTALLDIR)/orphlib.a
#
stopx: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o stopx
.f.o:
	$(F77) $(FFLAGS) -c $<
install: stopx
	install -d $(INSTALLDIR)
	install -p stopx $(INSTALLDIR)
	rm -f *.exe  stopx
	install -p stopx.hep $(INSTALLDIR)
