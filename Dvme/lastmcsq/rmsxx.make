SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = vmic6015.h devices.h datel.h rmsvme.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

rmsxx.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) rmsxx.c
	$(OAC) rmsxx.c
	$(ASM) rmsxx-o:NOLC-c:68040,=rmsxx.asm
	$(LINK) rmsxx.bin-r-t:"gogo",,=rmsxx,$(OALIB)
	-rm -f rmsxx.run
	-rm rmsxx.asm
	-rm rmsxx.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

