#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= swapo.o
#
LIBS= ../Dswapolib/swapolib.a\
      $(INSTALLDIR)/milibb.a\
      $(INSTALLDIR)/orphlib.a
#
swapo: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o swapo
.f.o:
	$(F77) $(FFLAGS) -c $<
install: swapo
	install -d $(INSTALLDIR)
	install -p swapo $(INSTALLDIR)
	rm -f *.exe  swapo
