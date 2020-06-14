FFLAGS= -O2
#
DIRA= /usr/hhirf/
DIRB= /tera/mcsq/Dlinux/Dvme/
DIRC= /tera/mcsq/Dlinux/Dacq/
#
LIBS= $(DIRA)orphlib.a $(DIRB)vmexxlib.a $(DIRC)vmelib.a
caenr: caenr.f $(LIBS) $(INCS)
	f77 $(FFLAGS) caenr.f $(LIBS) -o caenr
