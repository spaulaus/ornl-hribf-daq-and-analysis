FFLAGS= -O2
#
DIRA= /usr/hhirf/
DIRB= ./
DIRC= ../Dacq/
#
LIBS= $(DIRA)orphlib.a $(DIRB)vmexxlib.a $(DIRC)vmelib.a
sisread: sisread.f $(LIBS) $(INCS)
	f77 $(FFLAGS) sisread.f $(LIBS) -o sisread
