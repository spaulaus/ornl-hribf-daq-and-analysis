
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme_boot60.run: vme_boot60.s
	$(ASM) vme_boot60-c:68060,=vme_boot60.s
	-rm -f vme_boot60.run
	$(LINK) vme_boot60=vme_boot60
	-rm vme_boot60.obj
