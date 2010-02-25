
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme7_boot.run: vme7_boot.s
	$(ASM) vme7_boot-c:68040,=vme7_boot.s
	-rm -f vme7_boot.run
	$(LINK) vme7_boot=vme7_boot
	-rm vme7_boot.obj
