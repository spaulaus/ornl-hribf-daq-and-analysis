milibb:\
milibb.a(equate.o)\
milibb.a(helpman.o)\
milibb.a(his_close.o)\
milibb.a(isval.o)\
milibb.a(kaseup.o)\
milibb.a(kindel.o)\
milibb.a(kvalu.o)\
milibb.a(kvalue.o)\
milibb.a(looper.o)\
milibb.a(lwdmod.o)\
milibb.a(modfin.o)\
milibb.a(refmt.o)\
milibb.a(symlog.o)\
milibb.a(symsav.o)
	ranlib milibb.a
	@echo milibb is up-to-date
#
FFLAGS= $(OPT)  $(FARGS)
#
.f.o:
	$(F77) -c $(FFLAGS) $<
#
install: milibb
	install -d $(INSTALLDIR)
	install -p milibb.a $(INSTALLDIR)
