SOURCES = system.c  vmeprom.s lan_lib.c

INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ansi -c -S -68040 -O -useDS

opsys.obj:  $(SOURCES) $(INCS)
	$(OAC) system.c
	$(ASM) system-o:NOLC-c:68040,=system.asm
	-rm system.asm
	$(OAC) lan_lib.c
	$(ASM) lan_lib-c:68040,=lan_lib.asm
	-rm lan_lib.asm
	$(ASM) vmeprom-c:68040,=vmeprom.s
	$(LINK) opsys-d-r-e,opsys-u=system,vmeprom,lan_lib
	-rm lan_lib.obj
	-rm vmeprom.obj
	-rm system.obj
