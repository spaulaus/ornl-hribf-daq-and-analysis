
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme6_boot.run: vme6_boot.s
	$(ASM) vme6_boot-c:68040,=vme6_boot.s
	-rm -f vme6_boot.run
	$(LINK) vme6_boot=vme6_boot
	-rm vme6_boot.obj
