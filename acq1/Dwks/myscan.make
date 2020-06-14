DIRA=  /usr/acq/wks/
DIRB=  /usr/hhirf/
LDF = -laio -lpthreads -lmach -lc_r

OBJS= $(DIRA)scanuv.o $(DIRB)bansup2.o myscan.o

LIBS= $(DIRA)scanuvlib.a $(DIRB)scanulib.a $(DIRA)acqlib.a \
      $(DIRB)orphlib.a $(DIRA)ipclib.a

myscan: $(OBJS) $(LIBS)
	gfortran $(FFLAGS) $(OBJS) $(LIBS) $(LDF) -o myscan

