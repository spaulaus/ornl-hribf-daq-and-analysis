BASE=../
INCDIR=$(BASE)/include
LIBDIR=$(BASE)/lib
OBJS = vmecaen.o vmesis.o vmeio.o
CFLAGS = -O -I $(INCDIR) 

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

cleanall:
	rm -f vmexxlib.a $(OBJS)

ran:
	ranlib vmexxlib.a

