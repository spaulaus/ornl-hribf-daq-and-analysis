
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme4_boot.run: vme4_boot.s
	$(ASM) vme4_boot-c:68040,=vme4_boot.s
	-rm -f vme4_boot.run
	$(LINK) vme4_boot=vme4_boot
	-rm vme4_boot.obj
