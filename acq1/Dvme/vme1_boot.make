
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme1_boot.run: vme1_boot.s
	$(ASM) vme1_boot-c:68040,=vme1_boot.s
	-rm -f vme1_boot.run
	$(LINK) vme1_boot=vme1_boot
	-rm vme1_boot.obj
