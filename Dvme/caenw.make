FFLAGS= -O2
#
DIRA= /usr/hhirf/
DIRB= ./
DIRC= ../Dacq/
#
LIBS= $(DIRA)orphlib.a $(DIRB)vmexxlib.a $(DIRC)vmelib.a
caenw: caenw.f $(LIBS) $(INCS)
	f77 $(FFLAGS) caenw.f $(LIBS) -o caenw
