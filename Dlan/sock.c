


#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/utsname.h>
#include <net/if.h>
#include <netinet/in.h>
#include <linux/if_ether.h>
#include <net/if_arp.h>
# include <netpacket/packet.h>

int main()
{
  int tmo = 1000;
  char *dev,errbuf[256],device[16] = "eth1";
  int handle;

dev = device;
  printf ("Device: %s\n",dev);
  handle = pcap_open_live(dev,1510,0,tmo,errbuf);
while(1);
  return(0);

}

int 
pcap_open_live(char *device, int snaplen, int promisc, int to_ms, char *ebuf)
{
	int			sock_fd = -1, device_id, mtu, arptype;
	struct packet_mreq	mr;
static unsigned char multiaddr[] = {0x03,0x6d,0x63,0x73,0x71,0x00};

		sock_fd =  socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
		if (sock_fd == -1) {
			snprintf(ebuf, 256, "socket: %s",
				strerror(errno) );
			exit(99);
		}

			memset(&mr, 0, sizeof(mr));
			mr.mr_ifindex = 3;
			mr.mr_type    =  PACKET_MR_MULTICAST;
                        mr.mr_alen = 6;
                        memcpy(mr.mr_address,multiaddr,6);
			if (setsockopt(sock_fd, SOL_PACKET, 
				PACKET_ADD_MEMBERSHIP, &mr, sizeof(mr)) == -1)
			{
				snprintf(ebuf, 256, 
					"setsockopt: %s", strerror(errno));
				exit(99);;
			}
	return 1;

}

