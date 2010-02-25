SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = devices.h vmexx.h caen.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

vmexx.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) vmexx.c
	$(OAC) vmexx.c
	$(ASM) vmexx-o:NOLC-c:68040,=vmexx.asm
	$(LINK) vmexx.bin-r-t:"gogo",,=vmexx,$(OALIB)
	-rm -f vmexx.run
	-rm vmexx.asm
	-rm vmexx.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

