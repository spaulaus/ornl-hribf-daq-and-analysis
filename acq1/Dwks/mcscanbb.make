LDF = -laio -lpthreads -lmach -lc_r
DIRA=  /usr/hhirf/
DIRB=  /usr/acq/wks/
OBJS= scanuv/scanuv.o $(DIRA)bansup2.o mcscanbb.o
LIBS= scanuv/scanuvlib.a $(DIRA)scanulib.a acqlib.a $(DIRB)orphlib.a\
      ../Dshm/ipclib.a

mcscanbb: $(OBJS) $(LIBS)
	gfortran -O4 $(OBJS) $(LIBS) $(LDF) -o mcscanbb
