#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= fex.o  
#
LIBS= $(INSTALLDIR)/milibb.a\
      $(INSTALLDIR)/orphlib.a
#
fex: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o fex
.f.o:
	$(F77) $(FFLAGS) -c $<
install: fex
	install -d $(INSTALLDIR)
	install -p fex $(INSTALLDIR)
	rm -f *.exe  fex
	install -p fex.hep $(INSTALLDIR)
