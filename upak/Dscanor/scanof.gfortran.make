#
#  Use this Makefile to compile and link a scan for data analysis offline
#  This version is for the GNU gfortran.  
#  Note: -L/sw/lib is a Mac OS X FINK requirement.  Please modify or 
#        delete as needed.
FFLAGS= -O2 -fno-automatic -fsecond-underscore -fno-range-check
#
OBJS= /usr/hhirf/scanof.o\
      scanusubs.o
#
LIBS= /usr/hhirf/scanorlib.a\
      /usr/hhirf/orphlib.a
#
scanof: $(OBJS) $(LIBS)
#
	gfortran $(FFLAGS) $(OBJS) $(LIBS)  -o scanof
.f.o:
	gfortran $(FFLAGS) -c $<
