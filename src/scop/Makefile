#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= scop.o\
      epics/fepics.o\
      epics/dslib.o
#
LIBS= lib/scoplib.a\
      $(hhirfdir)/dammlib.a\
      $(hhirfdir)/xglib.a\
      $(hhirfdir)/dammlib.a\
      $(hhirfdir)/xglib.a\
      $(hhirfdir)/orphlib.a\
      $(libdir)/vmelib.a
#
scop: $(OBJS) lib/scoplib.a
#
	$(FORT) $(FFLAGS) -o scop $(OBJS) $(LIBS) -L/usr/X11R6/lib -lX11 -lpthread -lm

lib/scoplib.a:
	$(MAKE) -C lib 

install: scop
	install scop $(bindir)

clean:
	rm -f scop *.o lib/scoplib.a epics/*.o
#
.f.o:
	$(FORT) $(FFLAGS) -c $<
