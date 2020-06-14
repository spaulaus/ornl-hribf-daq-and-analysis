/*  addr_reso.c
*   functions to load and return components of the Ethernet
*   physical address database being maintained for the ORPH vme to
*   DECStation protocols.
*/

#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "../Dvme/orph.h"

#define LINELENGTH 80
#define MAX_ORPH_NODES 15

/*   Function Prototype   */
static int load_ORPH_addrs(void);

static struct node_addr {
       unsigned char name[12];
       unsigned char interface[6];
       unsigned char addr[6];
       } ORPH_addrs[MAX_ORPH_NODES]; 
static int number_of_nodes=0;  /* number of nodes actually in the table */

/*====================================================================*/
/*
*  name_to_addr function looks in the ORPH_addrs structure for the
*  address, having been given a name
*/
static unsigned char *en_name_to_addr(char *name)
{
    int i;

    if (number_of_nodes == 0)
      {
        number_of_nodes=load_ORPH_addrs();
        if (number_of_nodes < 0) return NULL;
      }
    if (strlen(name) == 0) return NULL;
    for (i=0; i<number_of_nodes ; ++i)
        if (strstr((char *)ORPH_addrs[i].name, name) != NULL)
             return ORPH_addrs[i].addr;

    return NULL;    /*  No name match found */
}

/*====================================================================*/
/*
*   name_to_interf function looks in the ORPH_addrs structure for the
*   interface on which a node is connected, having been given a name
*/
static unsigned char *en_name_to_interf(char *name)
{
    int i;

    if (number_of_nodes == 0)
      {
        number_of_nodes=load_ORPH_addrs();
        if (number_of_nodes < 0) return NULL;
      }
    if (strlen(name) == 0) return NULL;
    for (i=0; i<number_of_nodes ; ++i)
        if (strstr((char *)ORPH_addrs[i].name, name) != NULL)
            return ORPH_addrs[i].interface;

    return NULL;    /*  No name match found */
}

/*===================================================================*/
/*
*   load_ORPH_addrs loads the in memory structure of ORPH_addrs from
*   a file of hostnames and addresses.
*   The file format is expected to be:
*          #
*          #
*          #     comment lines
*          #
*          hostname: interface xx-xx-xx-xx-xx-xx
*             (as many lines as needed)
*/

static int load_ORPH_addrs(void)
{
    char *hostfile="/usr/acq/etc/ORPH_nodes.lst";
    FILE *fp;
    int i;
    int num_of_nodes;
    char inps[80];

    num_of_nodes = -1;
/*
*    Open the file, with precoded name
*/
    if ((fp=fopen(hostfile, "r")) == NULL)
     {
       printf("load_ORPH_addrs: can't open %s\n",hostfile);
       return -1;
     }
    else
     {
/*
*     Read lines from the hosts file
*/
       while ((int) (fgets(inps,LINELENGTH,fp)) != NULL)

         if (inps[0] != '#')
          {
/*
*            get node name
*/
            strcpy((char *)ORPH_addrs[++num_of_nodes].name,strtok(inps,":"));
            strcpy((char *)ORPH_addrs[num_of_nodes].interface,strtok(NULL," "));
            for (i=0; i<HW_ADDR_LEN; i++)
             {
               ORPH_addrs[num_of_nodes].addr[i]=
                                              strtol(strtok(NULL,"-"),NULL,16);
             }
          }
     }
    fclose(fp);
    return num_of_nodes+1;
}
/*****************************************************************************
*  Routine returns a pointer to the name of the default VME cpu.
*  
*****************************************************************************/
static char *en_default_node(void)
{
    int i;
    unsigned char *default_addr;

    if (number_of_nodes == 0)
      {
        number_of_nodes=load_ORPH_addrs();
        if (number_of_nodes < 0) return NULL;
      }
    default_addr = en_name_to_addr("vme");
    if (default_addr == NULL) return NULL;
    for (i=0; i < number_of_nodes; i++)
      {
        if (memcmp(ORPH_addrs[i].addr,default_addr,6) == 0)
         {
           if (strcmp((char *)ORPH_addrs[i].name,"vme") != 0) break;
         }
      }
    if (i == number_of_nodes) return NULL;
    return (char *)ORPH_addrs[i].name;
}
/****************************************************************************
*
*   Return the real node name for the default VME cpu
*
*   WARNING: The FORTRAN variable MUST be type CHARACTER!
****************************************************************************/
void default_vme_(char *name,int len)
{
   int  i;
   char *cptr;

   cptr = name;
   for (i=0; i < len; i++) *cptr++ = ' ';
   cptr = en_default_node();
   if (cptr != NULL)
     {
       strcpy(name,cptr);
       i = strlen(name);
       name[i] = ' ';
     }
}
