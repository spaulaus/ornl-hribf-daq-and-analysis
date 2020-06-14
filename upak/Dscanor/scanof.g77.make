#
# Use this Makefile to compile a scan for offline data analysis
# This version is for g77 compilers
#
FFLAGS= -O2 -fno-automatic
#
OBJS= /usr/hhirf/scanof.o\
      scanusubs.o
#
LIBS= /usr/hhirf/scanorlib.a\
      /usr/hhirf/orphlib.a
#
scanof: $(OBJS) $(LIBS)
#
	f77 $(FFLAGS) $(OBJS) $(LIBS)  -o scanof
.f.o:
	f77 $(FFLAGS) -c $<
