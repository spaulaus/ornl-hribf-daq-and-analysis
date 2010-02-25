LDF =
DIRA= /usr/hhirf/
DIRB= /usr/acq/wks/
OBJS= $(DIRA)scanor.o mcscanaa.o

LIBS= $(DIRA)scanorlib.a $(DIRB)acqlib.a $(DIRA)orphlib.a\
      $(DIRB)ipclib.a

mcscanaa: $(OBJS) $(LIBS)
	gfortran -O4 $(OBJS) $(LIBS) $(LDF) -o mcscanaa
