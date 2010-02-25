FILES = mem_mgr60.run lance60.run lan_str60.run rx_vmeprom60.run opsys.obj

SOURCES = mem_mgr.c lance.c lan_str.c lan_int60.s lan_lib.c dma_int.s \
           swap_copy.s gogoc.s

INCS = lan.h vme_sys.h orph.h mem_mgr.h vmeprom.h syram.h tcb.h devices.h

LIB = +/usr/oasys2/68040o/libansi,+libind

LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

OAC = /usr/oasys2/gcc68 -ansi -c -S -68060 -O -useDS -DCPU60

memmgr60.run:  $(FILES) $(SOURCES) $(INCS) 
	combine memmgr60 mem_mgr60 lance60 rx_vmeprom60 lan_str60

opsys.obj:  system.c vmeprom.s lan_lib.c $(INCS)
	make -f system.make

lance60.run:  lance.c lan_int60.s dma_int.s swap_copy.s $(INCS)
	$(OAC) lance.c
	$(ASM) lance-c:68060,=lance.asm
	$(ASM) lan_int60-c:68060,=lan_int60.s
	$(ASM) dma_int-c:68060,=dma_int.s
	$(ASM) swap_copy-c:68060,=swap_copy.s
	$(ASM) vmeprom-c:68060,=vmeprom.s
	$(LINK) lance60-o1:xA002,,=lance,lan_int60,dma_int,swap_copy,vmeprom,$(LIB)
	-rm lance.asm
	-rm lance.obj
	-rm lan_int60.obj
	-rm dma_int.obj
	-rm swap_copy.obj
	-rm vmeprom.obj

lan_str60.run:  lan_str.c opsys.obj $(INCS)
	$(OAC) lan_str.c
	$(ASM) lan_str-c:68060,=lan_str.asm
	-rm lan_str.asm
	$(ASM) vmeprom-c:68060,=vmeprom.s
	$(LINK) lan_str60-o1:x8002-t:x8002,,=lan_str,vmeprom,$(LIB)
	-rm lan_str.obj
	-rm vmeprom.obj

mem_mgr60.run: mem_mgr.c opsys.obj gogoc.s $(INCS)
	$(OAC) mem_mgr.c
	$(ASM) mem_mgr-c:68060,=mem_mgr.asm
	-rm mem_mgr.asm
	$(ASM) gogoc-c:68060,=gogoc.s
	$(LINK)  mem_mgr60-o1:xE002,,=gogoc,mem_mgr,opsys-r,$(LIB)
	-rm mem_mgr.obj

rx_vmeprom60.run: $(SYS_OBJS) rx_vmeprom.s
	$(ASM) rx_vmeprom-c:68060,=rx_vmeprom.s
	$(LINK) rx_vmeprom60=rx_vmeprom
	-rm rx_vmeprom.obj

