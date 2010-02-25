#include <stdio.h>
#include "pcap.h"
#include "pcap-int.h"
int main()
{
  int tmo = 1000;
  char *dev,errbuf[PCAP_ERRBUF_SIZE],device[16] = "eth1";
  pcap_t *handle;
 
dev = device;
  printf ("Device: %s\n",dev);
  handle = pcap_open_live(dev,1510,0,tmo,errbuf);
  if(handle == NULL) return(0);
while(1);
  return(0);

}
