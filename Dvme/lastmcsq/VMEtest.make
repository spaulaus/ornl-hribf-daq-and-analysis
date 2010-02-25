SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = Acq_Params.h acq_ctl.h orphmsg.h

SOURCES =

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

VMEtest.bin: $(SOURCES) $(INCS) $(INCS) $(SYS_OBJS) VMEtest.c
	$(OAC) VMEtest.c
	$(ASM) VMEtest-c:68040,=VMEtest.asm
	$(LINK) VMEtest.bin-r-t:"gogo",,=VMEtest,$(OALIB)
	-rm -f VMEtest.run
	-rm VMEtest.asm
	-rm VMEtest.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

