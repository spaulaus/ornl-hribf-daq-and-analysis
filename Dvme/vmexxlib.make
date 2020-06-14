CFLAGS = -g

OBJS = vmecaen.o vmeio.o vme_io.o vmesis.o

vmexxlib.a: vmexxlib.a($(OBJS)) ../Dacq/vmelib.a

	ranlib vmexxlib.a

vmecaen.o: vmexx.h ../Dlan/orph_pf.h
vmesis.o: vmexx.h ../Dlan/orph_pf.h sis3820.h
vmeio.o: vmexx.h ../Dlan/orph_pf.h ../Dacq/pkt_io.h ../Dlan/vmedII.h
vme_io.o: vmexx.h ../Dlan/orph_pf.h

.PHONY: cleanobjs cleanall ran

cleanobjs:
	rm -f $(OBJS)

cleanall:
	rm -f vmexxlib.a $(OBJS)

ran:
	ranlib vmexxlib.a

