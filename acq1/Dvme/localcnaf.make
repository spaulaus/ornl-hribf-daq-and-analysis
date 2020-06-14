SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = ksc.h

SOURCES =

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

localcnaf.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) localcnaf.c
	$(OAC) localcnaf.c
	$(ASM) localcnaf-o:NOLC-c:68040,=localcnaf.asm
	$(LINK) localcnaf.bin-r-t:"gogo",,=localcnaf,$(OALIB)
	-rm -f localcnaf.run
	-rm localcnaf.asm
	-rm localcnaf.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

