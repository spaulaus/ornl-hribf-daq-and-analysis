FFLAGS= $(OPT) $(FARGS)
#
OBJS= fitu.o\
      usercmp.o\
      userfox.o
#
LIBS= ../Dfitulib/fitulib.a\
      $(INSTALLDIR)/dammlib.a\
      $(INSTALLDIR)/xglib.a\
      $(INSTALLDIR)/dammlib.a\
      $(INSTALLDIR)/xglib.a\
      $(INSTALLDIR)/milibb.a\
      $(INSTALLDIR)/orphlib.a
#
fitu: $(OBJS) $(LIBS)
#
	$(F77)  $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o fitu -L/usr/X11R6/lib -lX11 -lpthread
.f.o:
	$(F77)  $(FFLAGS) -c $<
install: fitu
	install -d $(INSTALLDIR)
	install -p fitu $(INSTALLDIR)
	rm -f *.exe  fitu 
	install -p fitu.hep $(INSTALLDIR)
	install -p fitu.hhirf.make $(INSTALLDIR)/fitu.make
	install -p userfox.f $(INSTALLDIR)
