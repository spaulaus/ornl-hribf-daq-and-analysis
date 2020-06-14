stopelib:\
stopelib.a(anat.o)\
stopelib.a(blank.o)\
stopelib.a(buflod.o)\
stopelib.a(cmpdedx.o)\
stopelib.a(cmpgen.o)\
stopelib.a(docman.o)\
stopelib.a(dodedx.o)\
stopelib.a(dubliv.o)\
stopelib.a(fit3.o)\
stopelib.a(hicrat.o)\
stopelib.a(indxf.o)\
stopelib.a(nucname.o)\
stopelib.a(plotdedx.o)\
stopelib.a(plotxyfil.o)\
stopelib.a(plotxylin.o)\
stopelib.a(plotxyrec.o)\
stopelib.a(plotxysym.o)\
stopelib.a(pstop.o)\
stopelib.a(rednorth.o)\
stopelib.a(she.o)\
stopelib.a(shelco.o)\
stopelib.a(snorth.o)\
stopelib.a(snuc.o)\
stopelib.a(stopit.o)\
stopelib.a(stopum.o)\
stopelib.a(sward.o)\
stopelib.a(swarhe.o)\
stopelib.a(sziegl.o)\
stopelib.a(zefcr.o)\
stopelib.a(zpef.o)
	ranlib stopelib.a
	@echo  stopelib.a is up-to-date
#
#
FFLAGS= $(OPT) -DPTRSIZE=$(PTRSIZE) $(FARGS)
#
.F.o:
	$(F77) -c $(FFLAGS) $<

.f.o:
	$(F77) -c $(FFLAGS) $<
#
