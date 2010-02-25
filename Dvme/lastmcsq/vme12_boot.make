
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme12_boot.run: vme12_boot.s
	$(ASM) vme12_boot-c:68040,=vme12_boot.s
	-rm -f vme12_boot.run
	$(LINK) vme12_boot=vme12_boot
	-rm vme12_boot.obj
