ROOT=../
INC=$(ROOT)/include
LIB=$(ROOT)/lib
BIN=$(ROOT)/bin

CFLAGS = -g -I$(INC) -Wall

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

pecamio.o: $(INC)/cnaf.h $(INC)/pkt_io_udp.h $(INC)/orph_udp.h
acq_vme_ctrl.o: $(INC)/acq_ctl.h $(INC)/pkt_io_udp.h $(INC)/orph_udp.h
pkt_io_udp.o: $(INC)/pkt_io_udp.h
spkt_io_udp.o: $(INC)/spkt_io_udp.h
load_acq_params.o: $(INC)/pkt_io_udp.h $(INC)/orph_udp.h\
                    $(INC)/mem_mgr.h

install: vmelib.a pkt_io_udp.h spkt_io_udp.h
	install vmelib.a $(LIB)
	install pkt_io_udp.h $(INC)
	install spkt_io_udp.h $(INC)

.PHONY: cleanobjs cleanall ran

cleanobjs:
	rm -f $(OBJS)

clean:
	rm -f vmelib.a $(OBJS)

ran:
	ranlib vmelib.a
