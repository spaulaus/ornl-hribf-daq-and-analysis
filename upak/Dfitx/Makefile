#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= fitx.o
#
LIBS= ../Dfitxlib/fitxlib.a\
      ../Dmilibb/milibb.a\
      ../Dorphlib/orphlib.a
#
fitx: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) $(OBJS) $(LIBS) $(FLIBS) -o fitx 
.f.o:
	$(F77) $(FFLAGS) -c $<
install: fitx
	install -d $(INSTALLDIR)
	install -p fitx $(INSTALLDIR)
	rm -f *.exe  fitx
	install -p fitx.hep $(INSTALLDIR)
