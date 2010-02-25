SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = caen.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

caen_raw.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) caen_raw.c
	$(OAC) caen_raw.c
	$(ASM) caen_raw-o:NOLC-c:68040,=caen_raw.asm
	$(LINK) caen_raw.bin-r-t:"gogo",,=caen_raw,$(OALIB)
	-rm -f caen_raw.run
	-rm caen_raw.asm
	-rm caen_raw.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

