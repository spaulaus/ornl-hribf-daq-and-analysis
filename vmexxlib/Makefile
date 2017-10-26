BASE=..
DIRA= /usr/hhirf
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib
OBJS = vmecaen.o vmeio.o vmesis.o
CFLAGS = -O -I $(INCDIR) -Wall
FFLAGS = -O2 -I$(INCDIR) -Wall

LIBCAEN= $(DIRA)/orphlib.a $(LIBDIR)/vmexxlib.a $(LIBDIR)/vmelib.a

vmexxlib.a: vmexxlib.a($(OBJS))

	ranlib vmexxlib.a

vmecaen.o: $(INCDIR)/vmexx.h $(INCDIR)/orph_udp.h
vmesis.o: $(INCDIR)/vmexx.h $(INCDIR)/orph_udp.h
vmeio.o: $(INCDIR)/vmexx.h $(INCDIR)/orph_udp.h $(INCDIR)/pkt_io_udp.h

install: vmexxlib.a
	install vmexxlib.a $(LIBDIR)

.PHONY: cleanobjs cleanall ran

cleanobjs:
	rm -f $(OBJS)

clean:
	rm -f vmexxlib.a $(OBJS)

ran:
	ranlib vmexxlib.a

#
#
caenr: caenr.f $(LIBCAEN) $(INCS)
	$(FORT) $(FFLAGS) caenr.f $(LIBCAEN) -o caenr
#
#
caenw: caenw.f $(LIBCAEN) $(INCS)
	$(FORT) $(FFLAGS) caenw.f $(LIBCAEN) -o caenw
