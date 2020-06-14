
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme_listen60.run:  vme_listen60.s
	$(ASM) vme_listen60-c:68060,=vme_listen60.s
	-rm -f vme_listen60.run
	$(LINK) vme_listen60=vme_listen60
	-rm vme_listen60.obj
