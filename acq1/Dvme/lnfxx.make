SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = vmic3113a.h vmic6015.h devices.h lnfvme.h acromag.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

lnfxx.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) lnfxx.c
	$(OAC) lnfxx.c
	$(ASM) lnfxx-o:NOLC-c:68040,=lnfxx.asm
	$(LINK) lnfxx.bin-r-t:"gogo",,=lnfxx,$(OALIB)
	-rm -f lnfxx.run
	-rm lnfxx.asm
	-rm lnfxx.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

