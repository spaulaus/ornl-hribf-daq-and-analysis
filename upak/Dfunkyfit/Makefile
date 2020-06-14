#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= funkyfit.o
#
LIBS= $(INSTALLDIR)/milibb.a\
      $(INSTALLDIR)/orphlib.a
#
funkyfit: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o funkyfit
.f.o:
	$(F77) $(FFLAGS) -c $<
install: funkyfit
	install -d $(INSTALLDIR)
	install -p funkyfit $(INSTALLDIR)
	rm -f *.exe  funkyfit
	install -p funkyfit.hep $(INSTALLDIR)
