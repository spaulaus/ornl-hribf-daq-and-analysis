SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = lrs.h fastbus.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

fastxx.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) fastxx.c
	$(OAC) fastxx.c
	$(ASM) fastxx-c:68040,=fastxx.asm
	$(LINK) fastxx.bin-r-t:"gogo",,=fastxx,$(OALIB)
	-rm -f fastxx.run
	-rm fastxx.asm
	-rm fastxx.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

