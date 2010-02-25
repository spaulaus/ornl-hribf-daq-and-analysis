
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme10_boot.run: vme10_boot.s
	$(ASM) vme10_boot-c:68040,=vme10_boot.s
	-rm -f vme10_boot.run
	$(LINK) vme10_boot=vme10_boot
	-rm vme10_boot.obj
