
LINK = /usr/oasys2/l68

ASM = /usr/oasys2/a30

sav_res.run: sav_res.s vmeprom.inc
	$(ASM) sav_res-c:68040,=sav_res.s
	-rm -f sav_res.run
	$(LINK) sav_res=sav_res
	-rm  sav_res.obj
