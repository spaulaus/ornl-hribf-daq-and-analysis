SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS =

SOURCES =

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

bios.bin: $(SOURCES) $(INCS) $(SYS_OBJS) bios.c
	$(OAC) bios.c
	$(ASM) bios-c:68040,=bios.asm
	$(LINK) bios.bin-r-t:"gogo",,=bios,$(OALIB)
	-rm -f bios.run
	-rm bios.asm
	-rm bios.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

