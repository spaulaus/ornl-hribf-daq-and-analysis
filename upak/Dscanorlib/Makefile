scanorlib:\
scanorlib.a(bantesti.o)\
scanorlib.a(bantestn.o)\
scanorlib.a(bilnam.o)\
scanorlib.a(ccstop.o)\
scanorlib.a(chekas.o)\
scanorlib.a(chilcon.o)\
scanorlib.a(chilum.o)\
scanorlib.a(chilum2.o)\
scanorlib.a(chilum3.o)\
scanorlib.a(chilun.o)\
scanorlib.a(chilun1.o)\
scanorlib.a(climfix.o)\
scanorlib.a(closeall.o)\
scanorlib.a(cmpinput.o)\
scanorlib.a(cmpread.o)\
scanorlib.a(cmpsetup.o)\
scanorlib.a(cmptape.o)\
scanorlib.a(comset.o)\
scanorlib.a(count1.o)\
scanorlib.a(count1c.o)\
scanorlib.a(count1cc.o)\
scanorlib.a(countncc.o)\
scanorlib.a(ddat.o)\
scanorlib.a(doscan.o)\
scanorlib.a(drread.o)\
scanorlib.a(drrexam.o)\
scanorlib.a(drrmake.o)\
scanorlib.a(drropen.o)\
scanorlib.a(drrsub.o)\
scanorlib.a(drrtest.o)\
scanorlib.a(endrr.o)\
scanorlib.a(exabstat.o)\
scanorlib.a(filopenc.o)\
scanorlib.a(filopenu.o)\
scanorlib.a(formev.o)\
scanorlib.a(formev3.o)\
scanorlib.a(freg2.o)\
scanorlib.a(gateman.o)\
scanorlib.a(gattesti.o)\
scanorlib.a(hd1d.o)\
scanorlib.a(hd2d.o)\
scanorlib.a(hd1d.o)\
scanorlib.a(hedloc.o)\
scanorlib.a(helpnit.o)\
scanorlib.a(hisnit.o)\
scanorlib.a(hissub.o)\
scanorlib.a(hissum_vm.o)\
scanorlib.a(initu.o)\
scanorlib.a(input.o)\
scanorlib.a(inputipc.o)\
scanorlib.a(ioerr.o)\
scanorlib.a(ipcopen.o)\
scanorlib.a(ishft.o)\
scanorlib.a(iswaf.o)\
scanorlib.a(ldfchek.o)\
scanorlib.a(ldfhan.o)\
scanorlib.a(ldfinp.o)\
scanorlib.a(ldfopen.o)\
scanorlib.a(ldfread.o)\
scanorlib.a(ldfswap.o)\
scanorlib.a(messo.o)\
scanorlib.a(near2.o)\
scanorlib.a(nuinp.o)\
scanorlib.a(phase1.o)\
scanorlib.a(putput.o)\
scanorlib.a(pvsetup.o)\
scanorlib.a(redmil.o)\
scanorlib.a(redmilh.o)\
scanorlib.a(reset.o)\
scanorlib.a(scanormsg.o)\
scanorlib.a(scanornit.o)\
scanorlib.a(scanstop.o)\
scanorlib.a(statman.o)\
scanorlib.a(stopnit.o)\
scanorlib.a(swapb.o)\
scanorlib.a(swapf.o)\
scanorlib.a(swapper.o)\
scanorlib.a(taphan.o)\
scanorlib.a(tapopen.o)\
scanorlib.a(tapread.o)\
scanorlib.a(udfevent.o)\
scanorlib.a(udfhan.o)\
scanorlib.a(udfinp.o)\
scanorlib.a(udfname.o)\
scanorlib.a(udfopen.o)\
scanorlib.a(udfread.o)\
scanorlib.a(umesso.o)\
scanorlib.a(unpackaa.o)\
scanorlib.a(unpackbb.o)\
scanorlib.a(unpackl.o)\
scanorlib.a(unpackl1.o)\
scanorlib.a(unpackl2.o)\
scanorlib.a(unpackl3.o)\
scanorlib.a(usend.o)\
scanorlib.a(usercmp.o)\
scanorlib.a(usersub1.o)\
scanorlib.a(usersub2.o)\
scanorlib.a(usersub3.o)\
scanorlib.a(vmetchek.o)\
scanorlib.a(vmetcheka.o)\
scanorlib.a(vmetchekb.o)\
scanorlib.a(vmetfind.o)\
scanorlib.a(vmetfor.o)\
scanorlib.a(vmetfor1.o)\
scanorlib.a(vmetget.o)\
scanorlib.a(zottum.o)
	ranlib scanorlib.a
	@echo scanorlib.a is up-to-date
#
FFLAGS= $(OPT) $(FARGS)
#
.f.o:
	$(F77) -c $(FFLAGS) $<
#
chilum2.o:
	$(F77) -c --std=legacy $(FFLAGS) chilum2.f
#
chilum3.o:
	$(F77) -c --std=legacy $(FFLAGS) chilum3.f
#
#	ar rvU scanorlib.a  $*.o
install: scanorlib
	install -d $(INSTALLDIR)
	install -p scanorlib.a $(INSTALLDIR)

clean:
	rm -f *.exe *.o scanorlib.a
