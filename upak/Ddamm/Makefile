#
FFLAGS= $(OPT)  $(FARGS)
#
OBJS = damm.o
#
LIBS  = ../Ddammlib/dammlib.a\
        ../Dxglib/xglib.a\
        ../Ddammlib/dammlib.a\
        ../Dxglib/xglib.a\
        ../Dmilibb/milibb.a\
        ../Dorphlib/orphlib.a
#
damm: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) -o damm $(OBJS) $(LIBS) $(FLIBS) -L/usr/X11R6/lib -lX11 -lpthread 
.f.o:
	$(F77) $(FFLAGS) -c $<
install: damm
	install -d $(INSTALLDIR)
	install -p damm $(INSTALLDIR)
	rm -f *.exe  damm 
	install -p damm.hep $(INSTALLDIR)
clean:
	rm -f *.exe  damm damm.o
