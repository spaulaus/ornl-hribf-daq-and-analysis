#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= charge.o  
#
LIBS= $(INSTALLDIR)/orphlib.a
#
charge: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o charge
.f.o:
	$(F77) $(FFLAGS) -c  $<
install: charge
	install -d $(INSTALLDIR)
	install -p charge $(INSTALLDIR)
	rm -f *.exe  charge

clean:
	rm -f *.exe  *.o charge
