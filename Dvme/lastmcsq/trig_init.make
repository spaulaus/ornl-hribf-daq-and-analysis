SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h

SYS_OBJS = opsys.obj

INCS = devices.h trigger.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

trig_init.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) trig_init.c
	$(OAC) trig_init.c
	$(ASM) trig_init-o:NOLC-c:68040,=trig_init.asm
	$(LINK) trig_init.bin-r-t:"gogo",,=trig_init,$(OALIB)
	-rm -f trig_init.run
	-rm trig_init.asm
	-rm trig_init.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

