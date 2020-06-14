
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme9_boot.run: vme9_boot.s
	$(ASM) vme9_boot-c:68040,=vme9_boot.s
	-rm -f vme9_boot.run
	$(LINK) vme9_boot=vme9_boot
	-rm vme9_boot.obj
