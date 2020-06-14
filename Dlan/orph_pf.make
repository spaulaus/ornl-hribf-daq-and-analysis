CFLAGS= -O

OBJS = en_open.o\
       en_write.o\
       en_read.o\
       en_close.o\
       en_tmo.o\
       addr_reso.o\
       get_dest.o\
       hislist.o\
       pktlist.o

orph_pf.a: orph_pf.a($(OBJS))
	ranlib orph_pf.a

addr_reso.o: orph_pf.h
en_open.o: orph_pf.h
en_read.o: orph_pf.h
en_write.o: orph_pf.h
hislist.o: orph_pf.h
pktlist.o: orph_pf.h

.PHONY: cleanobjs cleanall ranlib

cleanobj:
	rm -f $(OBJS)

cleanall:
	rm -f orph_pf.a $(OBJS)

ran:
	ranlib orph_pf.a
