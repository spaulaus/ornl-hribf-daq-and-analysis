#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= kineq.o  
#
LIBS= $(INSTALLDIR)/orphlib.a
#
kineq: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o kineq
#
.f.o:
	$(F77) -c $(FFLAGS) $<
install: kineq
	install -d $(INSTALLDIR)
	install -p kineq $(INSTALLDIR)
	rm -f *.exe  kineq
