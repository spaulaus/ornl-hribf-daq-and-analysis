hhirflib= /usr/hhirf
OBJS = vmecaen.o vmeio.o vmesis.o

CFLAGS = $(OPT) $(CARGS)
FFLAGS = $(OPT) $(FARGS)

CAENLIB= $(hhirflib)/orphlib.a $(libdir)/vmexxlib.a $(libdir)/vmelib.a

all: install

install: install-vmexxlib.a install-bins

vmexxlib.a: vmexxlib.a($(OBJS))
	ranlib vmexxlib.a

vmecaen.o: $(includedir)/vmexx.h $(includedir)/orph_udp.h
vmesis.o: $(includedir)/vmexx.h $(includedir)/orph_udp.h
vmeio.o: $(includedir)/vmexx.h $(includedir)/orph_udp.h $(includedir)/pkt_io_udp.h

#
#
caenr: caenr.f $(CAENLIB) $(includedir)
	$(FORT) $(FFLAGS) caenr.f $(CAENLIB) -o caenr
#
#
caenw: caenw.f $(CAENLIB) $(includedir)
	$(FORT) $(FFLAGS) caenw.f $(CAENLIB) -o caenw
#
sisread: sisread.f $(CAENLIB) $(includedir)
	$(FORT) $(FFLAGS) $< $(CAENLIB) -o $@
#
#
install-vmexxlib.a: vmexxlib.a
	install vmexxlib.a $(libdir)

install-bins: caenr caenw sisread
	install caenr $(bindir)
	install caenw $(bindir)
	install sisread $(bindir)

.PHONY: cleanobjs cleanall ran

clean:
	rm -f *.o vmexxlib.a caenr caenw sisread 

ran:
	ranlib vmexxlib.a

