# HRIBF Data Acquisition and Analysis Software
## Disclaimer
The basis for these repositories were pulled from 
[ORNL Physics Division Data Acquisition Support](https://www.phy.ornl.gov/computing/daqsupport.html).
**This distribution is IN NO WAY endorsed by ORNL, UT-Batelle, or any other agency.**

## Introduction
I and other colleagues often have trouble locating and installing this software. 
I am putting them all together in this repository so that we can download them 
easily via git. I have also made some updates to ensure that we're using 
gfortran and can compile with gcc 9+. I may possibly upgrade some of the scripts
to use cmake. Because these instructions are gnarly. 

## Setup Instructions
The original version of these instructions were written by K. Smith, S. Burcher, 
C. Thornsberry, D. Walter associated 
with the [UTK Nuclear Structure Group](http://www.phys.utk.edu/research/nuclear/exp-low-energy-nuclear-physics.html).

This document will provide step-by-step instructions on how to setup the ORHPAS acquisition software. Written to support 
Fedora 24 / CentOS 6.

## Install Standard Packages
The standard packages can be installed with the dnf:
```bash
sudo dnf install gcc-gfortran libX11-devel tcsh environment-modules tftp-server dhcp
```
We now setup a directory to build the orphas packages:
```bash
cd ~/
mkdir programs
cd programs

```
## Install UPAK
Install UPAK in the program directory just created. 
```bash
wget ftp://ftp.phy.ornl.gov/pub/upak/upak-src.tgz
tar xzf upak-src.tgz
cd upak
```
We now remove the object and archive files that were compiled elsewhere.
```bash
find . -name *.o -exec rm {} \;
find . -name *.a -exec rm {} \;
```
We can now compile and install the binaries and libraries:
```bash
make
cd ..
sudo mv hhirf/ /usr
```

## Install Acq2
1. Download the tarball
    ```bash
    cd ~/programs
    wget ftp://ftp.phy.ornl.gov/pub/upak/Linux/data-acquisition/acq2-29Dec2017.tgz
    tar xzf acq2-myriad.tgz -C acq2
    cd acq2
    ```
2. Remove all object and archive files: 
    ```bash
    find . -name *.o -exec rm {} \;
    find . -name *.a -exec rm {} \;
    rm -f bin/* lib/* etc/acq_priority
    ```
3. Build required vme libraries
    ```bash
    cd vmelib
    make clean
    make install
    cd ..
    
    cd vmexxlib
    make clean
    make install
    cd ..
    ```
4. Update `acqlib` Makefile by swapping f77 for gfortran. 
    ```bash
    #FORT = /usr/bin/f77
    FORT = /usr/bin/gfortran
    ```
5. Compile `acqlib`
```bash
make clean
make install
cd ..
```
5. Build required ipc library
```bash
cd ipclib
make clean
make install
cd ..
```
6. Update `pacor` to use gfortran. 
    ```bash
    #FORT = /usr/bin/f77
    FORT = /usr/bin/gfortran
    ```
7. Build `pacor` library
```bash
make
cd ..
```
Build pacor objects
Edit Makefile swapping f77 for gfortran. Look for line called FORT.
```bash
#FORT = /usr/bin/f77
FORT = /usr/bin/gfortran
```
Compile
```bash
make install
cd ..
```
Build the Dacq binaries
```bash
./makeall
make acq_priority
cp acq_priority ../bin
cp acq_priority ../etc
cd ..
```
Build pacman
```bash
cd pacman
make install
cd ..
```
Build scadlib
```bash
cd scad/lib
```
Edit Makefile swapping f77 for gfortran. Look for line called FORT.
```bash
#FORTRAN = /usr/bin/f77
FORTRAN = /usr/bin/gfortran
```
Compile
```bash
make clean
make
cd ..

```
Build scad
Edit Makefile swapping f77 for gfortran. Look for line called FORT.
```bash
#FORTRAN = /usr/bin/f77
FORTRAN = /usr/bin/gfortran 
```
Compile
```bash
make clean
make install
cd ..
```
Build scop/lib
```bash
cd scop/lib
```

Edit Makefile swapping f77 for gfortran. Look for line called FORT.
```bash
#FORT = /usr/bin/f77
FORT = /usr/bin/gfortran

```
Compile
```bash
make
cd ..
```
Build scop
Edit Makefile swapping f77 for gfortran. Look for line called FORT.
```bash
#FORT = /usr/bin/f77
FORT = /usr/bin/gfortran
```
Compile
```bash
make clean
make install
cd ..
```
Build tape/scatlib
```bash
cd tape/scatlib
```
Edit Makefile swapping f77 for gfortran. Look for line called FORT.
```bash
#FORT = /usr/bin/f77
FORT = /usr/bin/gfortran
```
Compile
```bash
make 
cd ..
```
Build tape
Edit Makefile swapping f77 for gfortran. Look for line called FORT.
```bash
#FORT = /usr/bin/f77
FORT = /usr/bin/gfortran
```
Compile
```bash
make clean
make install
cd ..
```
Build udptoipc
```bash
cd udptoipc
```
Compile
```bash
make clean
make install
cd ..
```
Build vmereset
```bash
cd vmereset
```
Compile
```bash
make clean
make install
cd ..
```
Build vmeterm
```bash
cd vmeterm
```
Compile
```bash
make
cp vmeterm ../bin/
cd ..
```
Install scripts to bin folder
```bash
cp scripts/* bin/
ln -s m_setup bin/modu_setup
```
Note: It appears that modu_setup has been replaced with m_setup. The link 
resolves this without having to make modifications to files.

Install acq2 (and do some tftp stuff)
```bash
mkdir /usr/acq2
cp -r bin/ lib/ etc/ doc/ /usr/acq2/
cd /usr/acq2/etc
chmod +s acq_priority
```

Make module file
```bash
cd $MODULESHOME/modulefiles
```

Create a module file
```bash
vi orphas
```
Put the following text in the module file
```bash
#%Module1.0
##
## modules orphas
##
## modulefiles/orphas
##
proc ModulesHelp {} {
   global version modroot
   puts stderr "orphas - Oak Ridge Acquisition"
}
module-whatis "Oak Ridge Acquisition"

set acq2topdir  /usr/acq2
set upaktopdir  /usr/hhirf

prepend-path PATH   	$upaktopdir
prepend-path LD_LIBRARY_PATH	$upaktopdir
prepend-path LIBRARY_PATH   	$upaktopdir

prepend-path PATH   	$acq2topdir/bin
prepend-path LD_LIBRARY_PATH	$acq2topdir/lib
prepend-path LIBRARY_PATH   	$acq2topdir/lib
```
Quit super user
Add module to .bash-profile
```bash
cd ~
vi .bash-profile
```
Add line:
```bash
module load orhpas
```

Note: Module files can be easily loaded and unloaded using module load and 
module unload. In addition, one can see what is loaded using module list and which additional modules are available 
with module avail.

Configure tftp Server
Copy the ralf file to the tftpboot folder and create a symbolic link:
```bash
su
cd ~/programs/acq2/rtems/vmeacq/o-optimize
cp vmeacq.ralf /var/lib/tftpboot/
cd /var/lib/tftpboot
ln -s vmeacq.ralf vmeacp.bin
```
Modify the firewall to permit tftp traffic:
```bash
firewall-cmd --add-service tftp
firewall-cmd --permanent --add-service tftp
```
Enable the tftp service at boot:
```bash
systemctl enable tftp.socket
systemctl daemon-reload
```

Install xinetd
Set location of file for tftp (ex. `/home/tftpboot`) in `/etc/xinetd.d/tftp`

File should look like:
```bash
Service tftp
{
    socket_type   	 = dgram
    protocol   	 = udp
    wait   		 = yes
    user   		 = nobody
    server   		 = /usr/sbin/in.tftpd
    server_args   	 = -s -vvv -p /home/tftpboot
#    server_args   	 = -s -vvv -p /var/lib/tftpboot
    disable   		 = no
#    server_args   	 = -p -s /home/tftpboot -v -u tftpd
#    disable   		 = yes
    per_source   	 = 11
    cps   		 = 100 2
    flags   		 = IPv4
}
```
Run xinetd
Configure dhcp Server
Edit `/etc/dhcp/dhcpd.conf` to add addresses for the MVME. A preconfigured file 
is available with many MVMEs listed. To manually add an MVME add the following 
lines, replacing XX, YY, and the MAC addresses AA:BB:CC:DD:EE:FF and 
00:11:22:33:44:55 to the correct values for the MVME you would like to 
communicate with:
```bash
ddns-update-style none;
subnet 192.168.13.0 netmask 255.255.255.0 {
	option routers 192.168.13.1;
}
host vmeXX {
	hardware ethernet AA:BB:CC:DD:EE:FF;
	fixed-address 192.168.13.YY;
	filename "vmeacq.bin";
}
host vmeXXb {
	hardware ethernet 00:11:22:33:44:55;
	fixed-address 192.168.13.YY;
	filename "vmeacq.bin";
}
``` 
For example, for vme54 you would have:
```bash
ddns-update-style none;
subnet 192.168.13.0 netmask 255.255.255.0 {
	option routers 192.168.13.1;
}
host vme54 {
	hardware ethernet 00:01:AF:2D:89:3C;
	fixed-address 192.168.13.5;
	filename "vmeacq.bin";
}
host vme54b {
	hardware ethernet 00:01:AF:2D:89:3D;
	fixed-address 192.168.13.5;
	filename "vmeacq.bin";
}
```
Install dhcp as service starting at boot.
```bash
systemctl enable dhcpd
systemctl daemon-reload
```

# Prepare to Acquire
Ensure that the MVME being used is listed in `/etc/hosts`, the following line 
should be present, where XX and YY and replaced with the correct values:
```bash
192.168.13.YY vmeXX.phys.ornl.gov vmeXX
```
For example the entry for vme54 would read:
```bash
192.168.13.5 vme54.phys.ornl.gov vme54
```
Note: vme60 and beyond are not currently registered in `acq2/ipclib/ipckeys.h`. 
Using any of these MVMEs will result in the following message when starting 
pacman:
```bash
Servername is: vme60
Processor vme60:  Nonexistent ACQ Resource set
```

To resolve this `ipckeys.h` needs to be extended to add the additional 
resource, this may fail if the memory addresses overlap. The acq2 package will 
then need to be recompiled and installed. We suggest instead using vme54 for all
 standalone systems. Simply swap the name in `/etc/hosts` to vme54 for any vme 
 module. For example vme60 which has IP address 192.168.13.12 would read:
```bash
192.168.13.5 vme54.phys.ornl.gov vme54 #Actually vme60
192.168.13.12 vme60.phys.ornl.gov vme60 #Actually vme60
```
(both lines are needed)
Configure the environment. As the daq user make an experiment folder and set the
 following environment variables:
```bash
mkdir exp
cd exp
export VME=vme54
export EXPT=test
```
Note: As discussed above we use vme54 for all standalone systems. Be sure that 
the `/etc/hosts` is configured correctly.
Start pacman:
```bash
pacman
```
Load the pac file (These files usually end in .pac, but no extension is needed 
when typing the command.):
```bash
pacor pacfile l
```

Be happy!

Auxiliary Items
To communicate with the MVME5500 over serial port, the following parameters should be set using minicom
Run minicom in setup mode
```bash
 minicom -s
```
Set the port to Com1 (probably ttyS0)
Set Baud rate to 9600, IRQ to 8, no parity and no hardware or software flow control
To setup ssh server, run:
```bash
systemctl start sshd.service
systemctl enable sshd.service
```