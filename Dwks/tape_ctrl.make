FFLAGS= -O2
#
DIRA= /usr/hhirf/
#
LIBS= $(DIRA)orphlib.a acqlib.a ../Dlan/orph_pf.a
tape_ctrl: tape_ctrl.f $(LIBS) $(INCS)
	gfortran $(FFLAGS) tape_ctrl.f $(LIBS) -o tape_ctrl
