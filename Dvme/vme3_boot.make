
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme3_boot.run: vme3_boot.s
	$(ASM) vme3_boot-c:68040,=vme3_boot.s
	-rm -f vme3_boot.run
	$(LINK) vme3_boot=vme3_boot
	-rm vme3_boot.obj
