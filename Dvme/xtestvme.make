SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = Acq_Params.h acq_ctl.h acromag.h

SOURCES = acpran.s

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ansi -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

xtestvme.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) xtestvme.c
	$(ASM) acpran-c:68040,=acpran.s
	$(OAC) xtestvme.c
	$(ASM) xtestvme-c:68040,=xtestvme.asm
	$(LINK) xtestvme.bin-r-t:"gogo",,=acpran,xtestvme,$(OALIB)
	-rm -f xtestvme.run
	-rm xtestvme.asm
	-rm xtestvme.obj
	-rm acpran.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

