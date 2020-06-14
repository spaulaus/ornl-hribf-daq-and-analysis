SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = Acq_Params.h acq_ctl.h acromag.h

SOURCES = acpran.s

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ansi -c -S -68060 -O -useDS -DCPU60

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

testvme60.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) testvme.c
	$(ASM) acpran-c:68060,=acpran.s
	$(OAC) testvme.c
	$(ASM) testvme-c:68060,=testvme.asm
	$(LINK) testvme60.bin-r-t:"gogo",,=acpran,testvme,$(OALIB)
	-rm -f testvme60.run
	-rm testvme.asm
	-rm testvme.obj
	-rm acpran.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

