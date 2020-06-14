#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= chil.o
#
LIBS= ../Dchillib/chillib.a\
      $(INSTALLDIR)/orphlib.a
#
chil: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o chil
.f.o:
	$(F77) $(FFLAGS) -c $<

install: chil
	install -d $(INSTALLDIR)
	install -p chil $(INSTALLDIR)
	rm -f *.exe  chil

clean:
	rm -f *.exe  *.o chil.o chil
