SYS_SOURCES = system.c vmeprom.s lan_lib.c

SYS_INCS = vme_sys.h system.h vmeprom.h tcb.h syram.h lan.h lan_lib.h orph.h

SYS_OBJS = opsys.obj

INCS = Acq_Params.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ansi -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

data_proc.bin: $(SOURCES) $(INCS) $(SYS_INCS) $(SYS_OBJS) data_proc.c
	$(OAC) data_proc.c
	$(ASM) data_proc-c:68040,=data_proc.asm
	$(LINK) data_proc.bin-r-t:"gogo",,=data_proc,$(OALIB)
	-rm -f data_proc.run
	-rm data_proc.asm
	-rm data_proc.obj

opsys.obj: $(SYS_SOURCES) $(SYS_INCS)
	make -f system.make

