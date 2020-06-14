FILES = mem_mgr.run lance.run lan_str.run rx_vmeprom.run opsys.obj

SOURCES = mem_mgr.c lance.c lan_str.c lan_int.s lan_lib.c dma_int.s swap_copy.s\
          gogoc.s

INCS = lan.h vme_sys.h orph.h mem_mgr.h vmeprom.h syram.h tcb.h devices.h

LIB = +/usr/oasys2/68040o/libansi,+libind

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ansi -c -S -68040 -O -useDS

memmgr.run:  $(FILES) $(SOURCES) $(INCS)
	combine memmgr mem_mgr lance rx_vmeprom lan_str

opsys.obj:  system.c vmeprom.s lan_lib.c $(INCS)
	make -f system.make

lance.run:  lance.c lan_int.s dma_int.s swap_copy.s $(INCS)
	$(OAC) lance.c
	$(ASM) lance-c:68040,=lance.asm
	$(ASM) lan_int-c:68040,=lan_int.s
	$(ASM) dma_int-c:68040,=dma_int.s
	$(ASM) swap_copy-c:68040,=swap_copy.s
	$(ASM) vmeprom-c:68040,=vmeprom.s
	$(LINK) lance-o1:xA002,,=lance,lan_int,dma_int,swap_copy,vmeprom,$(LIB)
	-rm lance.asm
	-rm lance.obj
	-rm lan_int.obj
	-rm dma_int.obj
	-rm swap_copy.obj
	-rm vmeprom.obj

lan_str.run:  lan_str.c opsys.obj $(INCS)
	$(OAC) lan_str.c
	$(ASM) lan_str-c:68040,=lan_str.asm
	-rm lan_str.asm
	$(ASM) vmeprom-c:68040,=vmeprom.s
	$(LINK) lan_str-o1:x8002-t:x8002,,=lan_str,vmeprom,$(LIB)
	-rm lan_str.obj
	-rm vmeprom.obj

mem_mgr.run: mem_mgr.c opsys.obj gogoc.s $(INCS)
	$(OAC) mem_mgr.c
	$(ASM) mem_mgr-c:68040,=mem_mgr.asm
	-rm mem_mgr.asm
	$(ASM) gogoc-c:68040,=gogoc.s
	$(LINK)  mem_mgr-o1:xE002,,=gogoc,mem_mgr,opsys-r,$(LIB)
	-rm mem_mgr.obj

rx_vmeprom.run: $(SYS_OBJS) rx_vmeprom.s
	$(ASM) rx_vmeprom-c:68040,=rx_vmeprom.s
	$(LINK) rx_vmeprom=rx_vmeprom
	-rm rx_vmeprom.obj
