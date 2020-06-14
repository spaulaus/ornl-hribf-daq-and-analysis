SYS_OBJS = opsys.obj
 
INCS = vme_sys.h syram.h orph.h devices.h ln_alarm.h

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ANSI -c -S -68040 -O -useDS

OALIB = gogoc,opsys-r,+/usr/oasys2/68040o/libansi,+libind

ln_alarm.bin: $(INCS) $(SYS_OBJS) ln_alarm.c
	$(OAC) ln_alarm.c
	$(ASM) ln_alarm-o:NOLC-c:68040,=ln_alarm.asm
	$(LINK) ln_alarm.bin-r-t:"gogo",,=ln_alarm,$(OALIB)
	-rm -f ln_alarm.run
	-rm ln_alarm.asm
	-rm ln_alarm.obj
