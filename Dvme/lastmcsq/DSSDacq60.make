SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = Acq_Params.h acq_ctl.h ksc.h lrs.h \
       lrs1190.h orphmsg.h trigger.h devices.h

OBJS = 

SOURCES =

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ansi -c -S -68060 -O -useDS -DCPU60 -DORNL

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

DSSDacq60.bin: $(SOURCES) $(INCS) $(SYS_OBJS) DSSDacq.c
	$(OAC) DSSDacq.c
	$(ASM) DSSDacq-o:NOLC-c:68060,=DSSDacq.asm
	$(LINK) DSSDacq60.bin-r-t:"gogo",,=DSSDacq,$(OALIB)
	-rm -f DSSDacq60.run
	-rm DSSDacq.asm
	-rm DSSDacq.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

