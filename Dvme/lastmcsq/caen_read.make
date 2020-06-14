SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = caen.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

caen_read.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) caen_read.c
	$(OAC) caen_read.c
	$(ASM) caen_read-o:NOLC-c:68040,=caen_read.asm
	$(LINK) caen_read.bin-r-t:"gogo",,=caen_read,$(OALIB)
	-rm -f caen_read.run
	-rm caen_read.asm
	-rm caen_read.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

