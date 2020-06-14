SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = caen.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

caen_set.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) caen_set.c
	$(OAC) caen_set.c
	$(ASM) caen_set-o:NOLC-c:68040,=caen_set.asm
	$(LINK) caen_set.bin-r-t:"gogo",,=caen_set,$(OALIB)
	-rm -f caen_set.run
	-rm caen_set.asm
	-rm caen_set.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

