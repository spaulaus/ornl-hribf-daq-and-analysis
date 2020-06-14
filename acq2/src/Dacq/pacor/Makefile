# Makefile
# Make pacor and its library, if required
#
FFLAGS= $(OPTS) $(FARGS)

LIBS= ./lib/pacorlib.a\
      $(hhirfdir)/orphlib.a\
      $(hhirfdir)/milibb.a\
      $(libdir)/vmelib.a
#
#
pacor: pacor.o lib/pacorlib.a
	$(FORT) $(FFLAGS) pacor.o $(LIBS) -o pacor
#
lib/pacorlib.a:
	$(MAKE) -C lib 
#
#
.f.o:
	$(FORT) $(FFLAGS) -c pacor.f
#
install: pacor
	install pacor $(bindir)
