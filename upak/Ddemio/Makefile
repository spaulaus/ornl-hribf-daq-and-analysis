#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= demio.o 
#
LIBS= $(INSTALLDIR)/orphlib.a
#
demio: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o demio
.f.o:
	$(F77) $(FFLAGS) -c $<
install: demio
	install -d $(INSTALLDIR)
	install -p demio $(INSTALLDIR)
