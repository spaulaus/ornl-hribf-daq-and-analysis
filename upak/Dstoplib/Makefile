stoplib:\
stoplib.a(anat.o)\
stoplib.a(blank.o)\
stoplib.a(buflod.o)\
stoplib.a(cmpdedx.o)\
stoplib.a(docman.o)\
stoplib.a(dodedx.o)\
stoplib.a(dubliv.o)\
stoplib.a(fit3.o)\
stoplib.a(hicrat.o)\
stoplib.a(indxf.o)\
stoplib.a(pack.o)\
stoplib.a(pstop.o)\
stoplib.a(rednorth.o)\
stoplib.a(she.o)\
stoplib.a(shelco.o)\
stoplib.a(snorth.o)\
stoplib.a(snuc.o)\
stoplib.a(stopit.o)\
stoplib.a(stopum.o)\
stoplib.a(sward.o)\
stoplib.a(swarhe.o)\
stoplib.a(sziegl.o)\
stoplib.a(unpack.o)\
stoplib.a(zefcr.o)\
stoplib.a(zpef.o)
	ranlib stoplib.a
	@echo  stoplib.a is up-to-date
#
FFLAGS= $(OPT) $(FARGS)
#
.f.o:
	$(F77) -c $(FFLAGS) $<
#
#	ar rv stoplib.a $*.o
