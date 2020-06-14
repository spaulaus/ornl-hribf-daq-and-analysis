FFLAGS= -O2
#
DIRA= /usr/hhirf/
DIRB= ../Dvme/
DIRC= ../Dacq/
#
LIBS= $(DIRA)orphlib.a $(DIRB)vmexxlib.a $(DIRC)vmelib.a
caenr: caenr.f $(LIBS) $(INCS)
	f77 $(FFLAGS) caenr.f $(LIBS) -o caenr
