SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = Acq_Params.h acromag.h ksc.h lrs.h ces.h mem_mgr.h

SOURCES = vmedis.c bus_fault.s

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

vmebug.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) vmebug.c
	$(OAC) vmebug.c
	$(ASM) vmebug-c:68040,=vmebug.asm
	$(OAC) vmedis.c
	$(ASM) vmedis-o:NOLC-c:68040,=vmedis.asm
	$(ASM) bus_fault-c:68040,=bus_fault.s
	$(LINK) vmebug.bin-r-t:"gogo",,=vmebug,bus_fault,vmedis,$(OALIB)
	-rm vmedis.asm
	-rm vmedis.obj
	-rm bus_fault.obj
	-rm -f vmebug.run
	-rm vmebug.asm
	-rm vmebug.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

