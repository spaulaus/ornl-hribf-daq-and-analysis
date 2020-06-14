CFLAGS = -O

OBJS = pecamio.o\
       fastbusio.o\
       libipberr.o\
       libcamerr.o\
       for_acq_ctrl.o\
       acq_vme_ctrl.o\
       pkt_io.o\
       load_acq_params.o

vmelib.a: vmelib.a($(OBJS))
	ranlib vmelib.a

pecamio.o: ../Dvme/cnaf.h pkt_io.h ../Dlan/vmedII.h ../Dlan/orph_pf.h
fastbusio.o: ../Dvme/fastbus.h pkt_io.h ../Dlan/vmedII.h ../Dlan/orph_pf.h
for_acq_ctrl.o:
acq_vme_ctrl.o: ../Dvme/acq_ctl.h pkt_io.h ../Dlan/vmedII.h ../Dlan/orph_pf.h
pkt_io.o: pkt_io.h ../Dlan/vmedII.h ../Dlan/orph_pf.h
load_acq_params.o: ../Dvme/mem_mgr.h pkt_io.h ../Dlan/vmedII.h\
                    ../Dlan/orph_pf.h

.PHONY: cleanobjs cleanall ran new

cleanobjs:
	rm -f $(OBJS)

cleanall:
	rm -f vmelib.a $(OBJS)

ran:
	ranlib vmelib.a
new: 
	cp ../Dlan/orph_pf.a vmelib.a
