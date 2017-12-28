#
#
FFLAGS= $(OPTS) $(CARGS)
#
OBJS= scad.o\
      scadmsg.o
#
LIBS= ./lib/scadlib.a\
      $(hhirfdir)/dammlib.a\
      $(hhirfdir)/xglib.a\
      $(hhirfdir)/dammlib.a\
      $(hhirfdir)/xglib.a\
      $(hhirfdir)/orphlib.a\
      $(libdir)/vmelib.a\
      $(libdir)/vmexxlib.a
#
scad: $(OBJS) lib/scadlib.a
	$(FORT) $(FFLAGS) -o scad $(OBJS) $(LIBS) -L/usr/X11R6/lib -lX11 -lpthread

lib/scadlib.a:
	$(MAKE) -C lib 


# Make has a broken implicit .f.o rule.
.f.o:
	$(FORT) $(FFLAGS) -c $<

install: scad
	install scad $(bindir)

clean:
	rm -f scad *.o
