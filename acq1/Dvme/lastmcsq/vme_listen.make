
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

vme_listen.run:  vme_listen.s
	$(ASM) vme_listen-c:68040,=vme_listen.s
	-rm -f vme_listen.run
	$(LINK) vme_listen=vme_listen
	-rm vme_listen.obj
