#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= stopi.o
#
LIBS= ../Dstoplib/stoplib.a\
      $(INSTALLDIR)/milibb.a\
      $(INSTALLDIR)/orphlib.a
#
stopi: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o stopi
.f.o:
	$(F77) $(FFLAGS) -c $<
install: stopi
	install -d $(INSTALLDIR)
	install -p stopi $(INSTALLDIR)
	rm -f *.exe  stopi
	install -p stopi.hep $(INSTALLDIR)
