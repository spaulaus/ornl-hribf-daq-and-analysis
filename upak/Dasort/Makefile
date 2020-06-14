#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= asort.o  
#
LIBS= $(INSTALLDIR)/orphlib.a
#
asort: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o asort
.f.o:
	$(F77) $(FFLAGS) -c $<
install: asort
	install -d $(INSTALLDIR)
	install -p asort $(INSTALLDIR)
	rm -f *.exe  asort

clean:
	rm -f *.exe  *.o, asort
