
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme2_boot.run: vme2_boot.s
	$(ASM) vme2_boot-c:68040,=vme2_boot.s
	-rm -f vme2_boot.run
	$(LINK) vme2_boot=vme2_boot
	-rm vme2_boot.obj
