#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= fatrump.o  
#
LIBS= ../Dfatrumplib/fatrumplib.a\
      $(INSTALLDIR)/orphlib.a
#
fatrump: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o fatrump
.f.o:
	$(F77) $(FFLAGS) -c $<
install: fatrump
	install -d $(INSTALLDIR)
	install -p fatrump $(INSTALLDIR)
	rm -f *.exe  fatrump
