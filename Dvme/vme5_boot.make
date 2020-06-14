
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme5_boot.run: vme5_boot.s
	$(ASM) vme5_boot-c:68040,=vme5_boot.s
	-rm -f vme5_boot.run
	$(LINK) vme5_boot=vme5_boot
	-rm vme5_boot.obj
