#
#   Master Makefile for HRIBF UPAK - W. T. Milner data analysis code
#
###-PTRSIZE-##
#  The pointer size depends on the architecture
#  Mac G3/G4 = 32
#  Mac G5 = 64
#  Mac/Intel Pentium = 32
#  Intel Pentium64 = 64
#  Dec ALPHA = 64
export PTRSIZE=64
#
###-BYTEORDER-###
#  The byte order is processor dependent
#  PowerPC - BIGENDIAN
#  Intel Pentium - LITTLEENDIAN
#  AXP - LITTLE ENDIAN
#  SPARC - ?
export FENDIAN = -DLITTLEENDIAN
#export FENDIAN = -DBIGENDIAN
#
#  Set the compiler and flags
#
#--MAC OS X with FINK G77
#export F77=g77
#export CC=gcc
#export OPT= -O
#export FARGS= $(FENDIAN) -DG77 -fno-automatic -fsecond-underscore
#export CARGS= $(FENDIAN)
#export FLIBS= -L/sw/lib
#export ARFLAGS=rv
#export INSTALLDIR= /usr/hhirf
#
#--Gfortran - must be gcc >=4.9  (gcc49 and gcc49-shlibs on Mac Fink)
# MacOS version 
#export F77= /sw/bin/gfortran-fsf-4.9
#export CC=/sw/bin/gcc-fsf-4.9
#export OPT= -O
#export FARGS= $(FENDIAN) -DGFORTRAN -fno-automatic -fsecond-underscore -fno-range-check
#export CARGS= $(FENDIAN)
#export FLIBS= -L/sw/lib -lgfortran -lgcc
#export ARFLAGS=rv
#export INSTALLDIR= ../../hhirf
#
#--Gfortran - must be gcc >=4.2  
# LINUX version
export F77=gfortran
export CC=gcc
export OPT=-O
export FARGS= $(FENDIAN) -DGFORTRAN -fno-automatic -fsecond-underscore -fno-range-check
export CARGS= $(FENDIAN)
export ARFLAGS=rvU
export FLIBS= -lgfortran -lgcc
export INSTALLDIR= $(CURDIR)/hhirf
#
#--G77  
# LINUX version
#export F77=g77
#export CC=gcc
#export OPT=-O
#export FARGS= $(FENDIAN) -DG77 -fno-automatic -fsecond-underscore
#export CARGS= $(FENDIAN)
#export FLIBS=
#export ARFLAGS=rv
#export INSTALLDIR=/nfs_home/varner/hribf/hhirf
#
#--Cygwin, G77
#export F77= g77
#export CC=gcc
#export OPT= -O
#export FARGS= $(FENDIAN) -DG77 -fno-automatic -fsecond-underscore
#export CARGS= $(FENDIAN)
#export FLIBS=
#export ARFLAGS=rv
#export INSTALLDIR= /usr/hhirf

TARGETS= ipclib orphlib milibb xglib \
         asaplib asap asort \
         banco bando \
         charge chillib chil \
         dammlib damm demio \
         doc \
         fatrumplib fatrump fex fitulib fitu fitxlib fitx funkyfit \
         kineq \
         lemorlib lemor \
         ruth \
         scanorlib scanor \
         stopelib stope stoplib stopi stopo stopxlib stopx swapolib swapo \
         txxlib txx
.PHONY: clean all

all: $(TARGETS)

ipclib:
	cd D$@; \
	$(MAKE) install

orphlib:
	cd D$@; \
           $(MAKE) install

milibb:
	cd D$@; \
           $(MAKE) install

xglib:
	cd D$@; \
           $(MAKE) install

asaplib:
	cd D$@; \
           $(MAKE) install

asap:
	cd D$@; \
           $(MAKE) install

asort:
	cd D$@; \
           $(MAKE) install

banco:
	cd D$@; \
           $(MAKE) install

bando:
	cd D$@; \
           $(MAKE) install

charge:
	cd D$@; \
           $(MAKE) install

chillib:
	cd D$@; \
           $(MAKE) install

chil:
	cd D$@; \
           $(MAKE) install

dammlib:
	cd D$@; \
           $(MAKE) install

damm:
	cd D$@; \
           $(MAKE) install

demio:
	cd D$@; \
           $(MAKE) install

doc:
	cd D$@; \
           $(MAKE) install

fatrumplib:
	cd D$@; \
           $(MAKE)

fatrump:
	cd D$@; \
           $(MAKE) install

fex:
	cd D$@; \
           $(MAKE) install

fitulib:
	cd D$@; \
           $(MAKE)

fitu:
	cd D$@; \
           $(MAKE) install

fitxlib:
	cd D$@; \
           $(MAKE)

fitx:
	cd D$@; \
           $(MAKE) install

funkyfit:
	cd D$@; \
           $(MAKE) install

kineq:
	cd D$@; \
           $(MAKE) install

lemorlib:
	cd D$@; \
           $(MAKE) install

lemor:
	cd D$@; \
           $(MAKE) install

ruth:
	cd D$@; \
           $(MAKE) install

scanorlib:
	cd D$@; \
           $(MAKE) install

#    Scanor is special, only the object is installed
scanor:
	cd D$@; \
           $(MAKE) install

stopelib:
	cd D$@; \
           $(MAKE)

stope:
	cd D$@; \
           $(MAKE) install

stoplib:
	cd D$@; \
           $(MAKE)

stopi:
	cd D$@; \
           $(MAKE) install

stopo:
	cd D$@; \
           $(MAKE) install

stopxlib:
	cd D$@; \
           OPT="-g"; $(MAKE) 

stopx:
	cd D$@; \
           OPT="-g"; $(MAKE) install

swapolib:
	cd D$@; \
           $(MAKE) 

swapo:
	cd D$@; \
           $(MAKE) install

txxlib:
	cd D$@; \
           $(MAKE) 

txx:
	cd D$@; \
           $(MAKE) install

clean:
	rm -f */*.o */*.a */*.log

