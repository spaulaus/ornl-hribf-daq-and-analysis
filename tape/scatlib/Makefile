scatlibr:\
scatlibr.a(camerr.o)\
scatlibr.a(camred.o)\
scatlibr.a(compro.o)\
scatlibr.a(compum.o)\
scatlibr.a(kinop.o)\
scatlibr.a(labndx.o)\
scatlibr.a(norman.o)\
scatlibr.a(nxsym.o)\
scatlibr.a(scatcmp.o)\
scatlibr.a(scatcom.o)\
scatlibr.a(scatman.o)\
scatlibr.a(scatread.o)\
scatlibr.a(setpar.o)\
scatlibr.a(setup.o)\
scatlibr.a(setzecl.o)\
scatlibr.a(symbop.o)\
scatlibr.a(timekeep.o)\
scatlibr.a(vmeclr.o)\
scatlibr.a(vmered.o)\
scatlibr.a(zotum.o)
	ranlib scatlibr.a
	@echo scatlibr is up-to-date
#
FFLAGS= -O0 $(FARGS)
#
clean:
	rm -f  *.o scatlibr.a
.f.o:
	$(FORT) -c $(FFLAGS) $<
