SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = Acq_Params.h acq_ctl.h ces.h devices.h ksc.h lrs.h lrs1821.h\
       lrs1190.h orphmsg.h trigger.h

OBJS = 

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

VMEskel.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) VMEskel.c
	$(OAC) VMEskel.c
	$(ASM) VMEskel-c:68040,=VMEskel.asm
	$(LINK) VMEskel.bin-r-t:"gogo",,=VMEskel,$(OALIB)
	-rm -f VMEskel.run
	-rm VMEskel.asm
	-rm VMEskel.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

