#ifndef  LAN_LIB_H_
#define  LAN_LIB_H_

#ifndef  ORPH_H_
#define  ORPH_H_
#include  "orph.h"
#endif

void lan_open(char ,char **,struct Ether_Packet **);
int  lan_read(int timeout,char **);
void lan_write(int );
void lan_reply(int ,char );
void lan_close(void);
void lan_ioctl(int ,void *);

#endif       /*  end  LAN_LIB_H_   */
