SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = Acq_Params.h acromag.h ksc.h lrs.h ces.h mem_mgr.h

SOURCES = vmedis.c bus_fault60.s

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68060 -O -useDS -DCPU60

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

vmebug60.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) vmebug60.c
	$(OAC) vmebug60.c
	$(ASM) vmebug60-c:68060,=vmebug60.asm
	$(OAC) vmedis.c
	$(ASM) vmedis-o:NOLC-c:68060,=vmedis.asm
	$(ASM) bus_fault60-c:68060,=bus_fault60.s
	$(LINK) vmebug60.bin-r-t:"gogo",,=vmebug60,bus_fault60,vmedis,$(OALIB)
	-rm vmedis.asm
	-rm vmedis.obj
	-rm bus_fault60.obj
	-rm -f vmebug60.run
	-rm vmebug60.asm
	-rm vmebug60.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

