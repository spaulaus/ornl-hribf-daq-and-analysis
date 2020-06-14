#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= stopo.o
#
LIBS= ../Dstoplib/stoplib.a\
      $(INSTALLDIR)/milibb.a\
      $(INSTALLDIR)/orphlib.a
#
stopo: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o stopo
.f.o:
	$(F77) $(FFLAGS) -c $<
install: stopo
	install -d $(INSTALLDIR)
	install -p stopo $(INSTALLDIR)
	rm -f *.exe  stopo
	install -p stopo.hep $(INSTALLDIR)
