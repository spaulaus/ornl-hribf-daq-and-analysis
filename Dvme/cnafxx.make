SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = ksc.h cnaf.h devices.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

cnafxx.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) cnafxx.c
	$(OAC) cnafxx.c
	$(ASM) cnafxx-o:NOLC-c:68040,=cnafxx.asm
	$(LINK) cnafxx.bin-r-t:"gogo",,=cnafxx,$(OALIB)
	-rm -f cnafxx.run
	-rm cnafxx.asm
	-rm cnafxx.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

