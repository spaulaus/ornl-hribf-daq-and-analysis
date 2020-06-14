#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= ruth.o  
#
ruth: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) -o ruth
.f.o:
	$(F77) $(FFLAGS) -c $<
install: ruth
	install -d $(INSTALLDIR)
	install -p ruth $(INSTALLDIR)
	rm -f *.exe  ruth
