#
FFLAGS= -O $(FARGS)
#
OBJS= asap.o
#
LIBS= ../Dasaplib/asaplib.a\
      $(INSTALLDIR)/milibb.a\
      $(INSTALLDIR)/orphlib.a
#
asap: $(OBJS) $(LIBS)
	$(F77) $(OPT) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o asap
.f.o:
	$(F77) $(OPT) $(FFLAGS)  -c $<
install: asap
	install -d $(INSTALLDIR)
	install -p asap $(INSTALLDIR)
	rm -f *.exe  asap
	install -p asap.hep $(INSTALLDIR)

clean:
	rm -f *.exe *.o asap
