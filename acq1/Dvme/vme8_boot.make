
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme8_boot.run: vme8_boot.s
	$(ASM) vme8_boot-c:68040,=vme8_boot.s
	-rm -f vme8_boot.run
	$(LINK) vme8_boot=vme8_boot
	-rm vme8_boot.obj
