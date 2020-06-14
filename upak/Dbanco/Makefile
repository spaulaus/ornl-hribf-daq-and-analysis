#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= banco.o  
#
LIBS= $(INSTALLDIR)/orphlib.a\
      $(INSTALLDIR)/milibb.a
#
banco: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o banco
.f.o:
	$(F77) $(FFLAGS) -c $<
install: banco
	install -d $(INSTALLDIR)
	install -p banco $(INSTALLDIR)
	rm -f *.exe  banco

clean:
	rm -f *.exe  *.o banco
