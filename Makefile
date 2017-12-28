SHELL=/bin/sh
#
#   Master Makefile for ORNL Physics orpas - W. T. Milner and J.M. McConnell
#   data acquisition system
#
#  Pathnames
export prefix=/home/rxv/acq2
export exec_prefix=$(prefix)
export libdir=$(exec_prefix)/lib
export includedir=$(prefix)/include
export bindir=$(prefix)/bin

###-PTRSIZE-##
#  The pointer size depends on the architecture
export PTRSIZE=64
#
###-BYTEORDER-###
#  The byte order is processor dependent
#  Intel - LITTLEENDIAN
export FENDIAN = -DLITTLEENDIAN
#
#  Set the compiler and flags
#
#--Gfortran - must be gcc >=4.9  (These are Fink installs.  Fix for your sys.)
# MacOS version 
#export FORT= /sw/bin/gfortran-fsf-6
#export CC=/sw/bin/gcc-fsf-6
#export OPT= -O
#export FARGS= $(FENDIAN) -DGFORTRAN -fno-automatic -fsecond-underscore -fno-range-check
#export CARGS= $(FENDIAN)
#export FLIBS= -L/sw/lib -lgfortran -lgcc
#
#--Gfortran - must be gcc >=4.2  
# LINUX version
export hhirfdir=/usr/local/hhirf
export FORT= gfortran
export CC=gcc
export OPT= -O
export FARGS= $(FENDIAN) -DGFORTRAN -Wall -fno-automatic -fsecond-underscore -fno-range-check
export CARGS= $(FENDIAN) -Wall -I $(includedir)
export FLIBS= -lgfortran -lgcc
#
#--Cygwin, G77
#export FORT= g77
#export CC=gcc
#export OPT= -O
#export FARGS= $(FENDIAN) -DG77 -fno-automatic -fsecond-underscore
#export CARGS= $(FENDIAN)
#export FLIBS=

TARGETS= acqlib ipclib vmelib vmexxlib \
         Dacq ipctotcp pacman \
         rtems/vmeacq \
         scad scop \
         tape udptoipc \
         vmereset vmeterm

.PHONY: clean all
.PHONY: $(TARGETS)

all: $(TARGETS)

$(TARGETS):
	$(MAKE) -C $@ install

clean: 
	rm $(bindir)/* $(includedir)/* $(libdir)/*
	$(MAKE) -C acqlib clean
	$(MAKE) -C ipclib clean
	$(MAKE) -C vmelib clean
	$(MAKE) -C vmexxlib clean
	$(MAKE) -C Dacq clean
	$(MAKE) -C ictotcp clean
	$(MAKE) -C pacman clean
	$(MAKE) -C scad clean
	$(MAKE) -C scop clean
	$(MAKE) -C tape clean
	$(MAKE) -C udptoipc clean
	$(MAKE) -C vmereset clean
	$(MAKE) -C vmeterm clean
	$(MAKE) -C rtems/vmeacq clean
