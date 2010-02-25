
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

sav_res60.run: sav_res60.s
	$(ASM) sav_res60-c:68060,=sav_res60.s
	-rm -f sav_res60.run
	$(LINK) sav_res60=sav_res60
	-rm  sav_res60.obj
