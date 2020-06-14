#
FFLAGS=  -fno-automatic
#
OBJS= asapred.o
#
LIBS=  ../Dorphlib/orphlib.a
#
asapred: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS) -o asapred
.f.o:
	f77 $(FFLAGS) -c $<
