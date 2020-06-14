SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = vmic6015.h devices.h datel.h lnfvme.h acromag.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

stopln.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) stopln.c
	$(OAC) stopln.c
	$(ASM) stopln-o:NOLC-c:68040,=stopln.asm
	$(LINK) stopln.bin-r-t:"gogo",,=stopln,$(OALIB)
	-rm -f stopln.run
	-rm stopln.asm
	-rm stopln.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

