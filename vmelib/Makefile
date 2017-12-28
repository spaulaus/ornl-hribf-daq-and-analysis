CFLAGS = $(OPT) $(CARGS)

OBJS = pecamio.o\
       libipberr.o\
       libcamerr.o\
       pkt_io_udp.o\
       spkt_io_udp.o\
       load_acq_params.o\
       acq_vme_ctrl.o\
       for_acq_ctrl.o\
       fastbusio.o

vmelib.a: vmelib.a($(OBJS))
	ranlib vmelib.a

pecamio.o: $(includedir)/cnaf.h $(includedir)/pkt_io_udp.h $(includedir)/orph_udp.h
acq_vme_ctrl.o: $(includedir)/acq_ctl.h $(includedir)/pkt_io_udp.h $(includedir)/orph_udp.h
pkt_io_udp.o: $(includedir)/pkt_io_udp.h
spkt_io_udp.o: $(includedir)/spkt_io_udp.h
load_acq_params.o: $(includedir)/pkt_io_udp.h $(includedir)/orph_udp.h\
                    $(includedir)/mem_mgr.h

install: vmelib.a pkt_io_udp.h spkt_io_udp.h
	install vmelib.a $(libdir)
	install pkt_io_udp.h $(includedir)
	install spkt_io_udp.h $(includedir)

.PHONY: cleanobjs cleanall ran

cleanobjs:
	rm -f $(OBJS)

clean:
	rm -f vmelib.a $(OBJS)

ran:
	ranlib vmelib.a
