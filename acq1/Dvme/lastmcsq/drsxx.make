SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = vmic6015.h vmic3128.h devices.h datel.h drsvme.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

drsxx.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) drsxx.c
	$(OAC) drsxx.c
	$(ASM) drsxx-o:NOLC-c:68040,=drsxx.asm
	$(LINK) drsxx.bin-r-t:"gogo",,=drsxx,$(OALIB)
	-rm -f drsxx.run
	-rm drsxx.asm
	-rm drsxx.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

