
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

rx_vmeprom.run: rx_vmeprom.obj
	$(LINK) rx_vmeprom=rx_vmeprom
	combine memmgr mem_mgr lance rx_vmeprom lan_str


rx_vmeprom.obj: rx_vmeprom.s
	$(ASM) rx_vmeprom-c:68040,=rx_vmeprom.s
	-rm -f rx_vmeprom.run
