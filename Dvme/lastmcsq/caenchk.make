SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = acromag.h ksc.h lrs.h ces.h trigger.h devices.h

SOURCES = dev_test.s

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -OM -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

OBJS = dev_test.obj


caenchk.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) caenchk.c
	$(OAC) caenchk.c
	$(ASM) caenchk-o:NOLC-c:68040,=caenchk.asm
	$(ASM) dev_test-c:68040,=dev_test.s
	$(LINK) caenchk.bin-r-t:"gogo",,=caenchk,dev_test,$(OALIB)
	-rm dev_test.obj
	-rm -f caenchk.run
	-rm caenchk.asm
	-rm caenchk.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

