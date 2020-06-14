
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme11_boot.run: vme11_boot.s
	$(ASM) vme11_boot-c:68040,=vme11_boot.s
	-rm -f vme11_boot.run
	$(LINK) vme11_boot=vme11_boot
	-rm vme11_boot.obj
