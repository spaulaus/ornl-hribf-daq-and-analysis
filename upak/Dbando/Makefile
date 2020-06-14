#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= bando.o  
#
LIBS= $(INSTALLDIR)/milibb.a\
      $(INSTALLDIR)/orphlib.a
#
bando: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o bando
.f.o:
	$(F77) $(FFLAGS) -c $<
install: bando
	install -d $(INSTALLDIR)
	install -p bando $(INSTALLDIR)
	rm -f *.exe  bando

clean:
	rm -f *.exe  *.o bando
