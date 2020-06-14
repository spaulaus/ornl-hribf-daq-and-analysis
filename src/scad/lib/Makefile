scadlib:\
scadlib.a(alarmtst.o)\
scadlib.a(blank.o)\
scadlib.a(camerrr.o)\
scadlib.a(camred.o)\
scadlib.a(camredd.o)\
scadlib.a(camsym.o)\
scadlib.a(cbsast.o)\
scadlib.a(cmprun.o)\
scadlib.a(cmpscad.o)\
scadlib.a(cmpscud.o)\
scadlib.a(comnit.o)\
scadlib.a(compro.o)\
scadlib.a(compum.o)\
scadlib.a(comset.o)\
scadlib.a(ctbsnit.o)\
scadlib.a(disp.o)\
scadlib.a(disprate.o)\
scadlib.a(dometer.o)\
scadlib.a(fillcir.o)\
scadlib.a(fli8.o)\
scadlib.a(flo10.o)\
scadlib.a(flo12.o)\
scadlib.a(flo6.o)\
scadlib.a(flo8.o)\
scadlib.a(glabndx.o)\
scadlib.a(glimset.o)\
scadlib.a(gnitter.o)\
scadlib.a(kinop.o)\
scadlib.a(labndx.o)\
scadlib.a(limdsp.o)\
scadlib.a(limsav.o)\
scadlib.a(limset.o)\
scadlib.a(logopen.o)\
scadlib.a(logum.o)\
scadlib.a(newfig.o)\
scadlib.a(norman.o)\
scadlib.a(numeter.o)\
scadlib.a(nxsym.o)\
scadlib.a(openerr.o)\
scadlib.a(readum.o)\
scadlib.a(resetx.o)\
scadlib.a(scadmsg.o)\
scadlib.a(scadnit.o)\
scadlib.a(sclr.o)\
scadlib.a(scrnit.o)\
scadlib.a(sendbuf.o)\
scadlib.a(setecl.o)\
scadlib.a(setpar.o)\
scadlib.a(setup.o)\
scadlib.a(snap.o)\
scadlib.a(statman.o)\
scadlib.a(symbop.o)\
scadlib.a(tabo.o)\
scadlib.a(zotum.o)\
scadlib.a(vmered.o)\
scadlib.a(vmeclr.o)\
scadlib.a(gsetecl.o)\
scadlib.a(gsetpar.o)\
scadlib.a(gvmered.o)

	ranlib scadlib.a
	@echo scadlib.a is up-to-date
#
FFLAGS= -O0 $(FARGS)
#
# Broken .f.o implicit rule
.f.o:
	$(FORT) $(FFLAGS) -c $<

clean:
	rm scadlib.a *.o
