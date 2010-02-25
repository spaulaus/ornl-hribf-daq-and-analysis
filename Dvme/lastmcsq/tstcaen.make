SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = vmic6015.h devices.h datel.h rmsvme.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

tstcaen.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) tstcaen.c caen.h
	$(OAC) tstcaen.c
	$(ASM) tstcaen-o:NOLC-c:68040,=tstcaen.asm
	$(LINK) tstcaen.bin-r-t:"gogo",,=tstcaen,$(OALIB)
	-rm -f tstcaen.run
	-rm tstcaen.asm
	-rm tstcaen.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

