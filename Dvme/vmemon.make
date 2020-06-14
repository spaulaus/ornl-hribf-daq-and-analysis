SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan_lib.h lan.h orph.h

SYS_OBJS = opsys.obj

INCS = acromag.h devices.h pit.h trigger.h

SOURCES =

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

vmemon.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) vmemon.c
	$(OAC) vmemon.c
	$(ASM) vmemon-o:NOLC-c:68040,=vmemon.asm
	$(LINK) vmemon.bin-r-t:"gogo",,=vmemon,$(OALIB)
	-rm -f vmemon.run
	-rm vmemon.asm
	-rm vmemon.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

