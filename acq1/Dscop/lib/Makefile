scoplib:\
scoplib.a(abortum.o)\
scoplib.a(beamline.o)\
scoplib.a(camerrr.o)\
scoplib.a(camred.o)\
scoplib.a(camsym.o)\
scoplib.a(cmpscop.o)\
scoplib.a(comnit.o)\
scoplib.a(displar.o)\
scoplib.a(disprate.o)\
scoplib.a(dometer.o)\
scoplib.a(fillcir.o)\
scoplib.a(glabndx.o)\
scoplib.a(glimset.o)\
scoplib.a(newfig.o)\
scoplib.a(numeter.o)\
scoplib.a(scopnit.o)\
scoplib.a(sendepic.o)\
scoplib.a(snitter.o)\
scoplib.a(statman.o)\
scoplib.a(winrat.o)
	ranlib scoplib.a
	@echo scoplib.a  is up-to-date
#
FFLAGS= -O0 -fno-automatic
#
.f.o:
	f77 -c $(FFLAGS) $<
#
	ar rv scoplib.a $*.o
