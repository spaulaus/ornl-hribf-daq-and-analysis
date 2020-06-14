swapolib:\
swapolib.a(cmdproc.o)\
swapolib.a(conv.o)\
swapolib.a(dread.o)\
swapolib.a(drite.o)\
swapolib.a(drrswap.o)\
swapolib.a(helplis.o)\
swapolib.a(hisswap.o)\
swapolib.a(ldfswap.o)\
swapolib.a(spkswap.o)\
swapolib.a(swapb.o)\
swapolib.a(swaph.o)\
swapolib.a(swapnit.o)\
swapolib.a(swapper.o)\
swapolib.a(swapum.o)
	ranlib swapolib.a
	@echo  swapolib.a is up-to-date
#
FFLAGS= $(OPT) $(FARGS)
#
.f.o:
	$(F77) -c $(FFLAGS) $<
#
#	ar rv swapolib.a $*.o
