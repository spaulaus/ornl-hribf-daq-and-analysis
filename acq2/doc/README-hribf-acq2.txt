15 March 2017
29 December 2017

Installation of the HRIBF data acquisition and analysis on Intel Linux
THESE INSTRUCTIONS ARE INCOMPLETE!i
Email varnerrl@ornl.gov if you want to use this software.

The installation requires two gzipped tar files.  The binaries are compiled 
with gcc and gfortran under RHEL7.  The files are:

acq2-29Dec2017.tgz
	Source code for the data acquisition and control
	Extraction: tar xzpf hribf-acq2.tar.gz --same-owner /
        The archive expands into a directory "acq2".

upak-linux-intel.tgz
	Source and Executable code for the UPAK software
	The binaries should be installed in /usr/hhirf
	Extraction: tar xzpf upak-linux-intel.tgz --same-owner /


To install an operational system, you need the acq2-29Dec2017.tgz
file and the upak-linux-intel.tgz file.  The tgz files contain the sources.

INSTALLATION

0a) Create the user, "hhirf" with UID=50061 and GID=wheel.  You may
choose any other UID and GID as you wish, but be sure that the files
are all changed to be owned by the new UID and GID.

1) as root, place the archive acq2-29Dec2017.tgz somewhere, then
linux> tar xzpvf /somewhere/acq2-29Dec2017.tgz --same-owner /

When done, you should have the directory "acq2".  For this to work 
without change, you should, as root, make a soft link,

ln -s ./acq2 /usr/acq2 

because pacman will look in /usr/acq2 for many files.  Be sure that /usr/acq2/bin 
is in your path.

In the directory acq2/etc one file 
is SUID root:

-rwsr-xr-x 1 root  root 14897 Jun 23 16:23 acq_priority
-rw-r--r-- 1 hhirf man   2032 Jun 23 16:23 pacman.fig
-rw-r--r-- 1 hhirf man     42 Jun 23 16:23 set-newacq.csh
-rw-r--r-- 1 hhirf man     50 Jun 23 16:23 set-newacq.sh


In acq2/bin, one file is owned by root and is suid,

-rwsr-xr-x  1 root root   11143 Jun 27 14:41 udpmon

  
2) as root, place the archive upak-linux.tgz somewhere, then
linux> tar xzpvf /somewhere/upak-linux.tgz, which creates ./upak. Link
the directory to 

ln -s upak/hhirf /usr/hhirf

When done, you should have the directory "/usr/hhirf". 

3) You also need to configure your workstation for DHCP/BOOTP, TFTPD 
and to install one or more boot files for the MVME5500 and you will need RTEMS.  
Look in the folder acq2/etc for configuration files for dhcpd and xinetd.d/tftp.  
Create the folder with name /home/tfptboot.  The "make" command in "acq2/src" 
should install the vmeacq in /home/tftpboot.  Otherwise, you can copy

cp -p src/rtems/vmeacq/o-optimize/vmeacq.ralf /home/tftpboot

Make sure the files are readable by tftp.
More documentation on that may come later.


For further assistance, email Robert Varner (varnerrl@ornl.gov).



