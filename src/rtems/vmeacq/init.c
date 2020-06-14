/*
 * RTEMS configuration/initialization
 * 
 * This program may be distributed and used for any purpose.
 * I ask only that you:
 *	1. Leave this author information intact.
 *	2. Document any changes you make.
 *
 * W. Eric Norum
 * Saskatchewan Accelerator Laboratory
 * University of Saskatchewan
 * Saskatoon, Saskatchewan, CANADA
 * eric@skatter.usask.ca
 *
 *  init.c,v 1.8.2.1 2003/09/18 20:47:37 joel Exp
 *  init.c 2005/03/09  rlv ORNL
 *    Adapt to cnafxx
 *
 *  This one for VMEacq
 */

#include <bsp.h>
#include "../include/devices.h"

#define CONFIGURE_APPLICATION_NEEDS_CONSOLE_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER
#define CONFIGURE_RTEMS_INIT_TASKS_TABLE
#define CONFIGURE_LIBIO_MAXIMUM_FILE_DESCRIPTORS 20
#define CONFIGURE_USE_IMFS_AS_BASE_FILESYSTEM

#define CONFIGURE_EXECUTIVE_RAM_SIZE	(512*1024)
#define CONFIGURE_MAXIMUM_SEMAPHORES	20
#define CONFIGURE_MAXIMUM_TASKS		20

#define CONFIGURE_MICROSECONDS_PER_TICK	10000

#define CONFIGURE_INIT_TASK_STACK_SIZE	(10*1024)
#define CONFIGURE_INIT_TASK_PRIORITY	100
#define CONFIGURE_INIT_TASK_INITIAL_MODES (RTEMS_PREEMPT | \
                                           RTEMS_NO_TIMESLICE | \
                                           RTEMS_NO_ASR | \
                                           RTEMS_INTERRUPT_LEVEL(0))

#define CONFIGURE_MAXIMUM_MESSAGE_QUEUES  4

#define CONFIGURE_INIT
rtems_task Init (rtems_task_argument argument);

#include <rtems/confdefs.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <rtems.h>
#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>
#include <bsp/vme_am_defs.h>
#include <bsp/VME.h>
#include <libcpu/bat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/time.h>
#include "../include/networkconfig.h"

extern rtems_task cnafxx (rtems_task_argument ignored);
extern rtems_task fastxx (rtems_task_argument ignored);
extern rtems_task vmexx (rtems_task_argument ignored);
extern rtems_task devchk (rtems_task_argument ignored);
extern rtems_task resetxx (rtems_task_argument ignored);
extern rtems_task VMEacq (rtems_task_argument ignored);
extern rtems_task memmgrxx (rtems_task_argument ignored);
extern rtems_task clkxx (rtems_task_argument ignored);
extern rtems_task data_proc (rtems_task_argument ignored);
extern rtems_task vmemon (rtems_task_argument ignored);
/*    Global data             */

struct devices DEVTBL;    /* Devices table */
rtems_id Que1id;          /* Message queue */
void *Que1buf;

unsigned long debugdata[1000]={1,2,3,4,5,6,7,8,9,0}; /* debugging data from acquisition */
unsigned long debugdata_index=0;

/* Display some IP statistics */
static void showStatistics (void)
{
  rtems_bsdnet_show_inet_routes ();
  rtems_bsdnet_show_mbuf_stats ();
  rtems_bsdnet_show_if_stats ();
  rtems_bsdnet_show_ip_stats ();
  rtems_bsdnet_show_icmp_stats ();
  rtems_bsdnet_show_udp_stats ();
  rtems_bsdnet_show_tcp_stats ();

  return;
}
/* Display some debugging data, in a large resevered memory*/
static void debugprint(void)
{
  int i,j;
  printf("Debugging data\n");
  for (i=0; i<debugdata_index; i+=5) {
    for (j=0;j<5;j++)
      printf("[%i]=%lx ",i+j, debugdata[i+j]);
    printf("\n");
  }
  return;
}
/* Zero the debugging data */
static void debugzero(void)
{
  int i;
  printf("Zeroing debug data\n");
  for (i=0; i<1000; i++) {
    debugdata[i]=0;
  }
    debugdata_index=0;
  return;
}

static void prettyPrintObjectId(rtems_id id)
{
  int tmpAPI, tmpClass;
  char tmpObjName[32];

  tmpAPI   = rtems_object_id_get_api(id),
  tmpClass = rtems_object_id_get_class(id),

  printf(
    "Name=%s API=%s Class=%s Node=%ld Index=%d\n",
    rtems_object_get_name(id, 32, tmpObjName),
    rtems_object_get_api_name(tmpAPI),
    rtems_object_get_api_class_name(tmpAPI, tmpClass),
    rtems_object_id_get_node(id),
    rtems_object_id_get_index(id)
  );
}


/*
 * RTEMS Startup Task
 */
rtems_task
Init (rtems_task_argument ignored)
{
  //rtems_id tidcan,tidfast,tidvme,tiddev,tidres,tidacq;
  rtems_id tidcan,tidvme,tiddev,tidres,tidacq;
  rtems_id tidmem,tidproc,tidclk,tidmon;
  rtems_status_code sc;
  rtems_task_priority my_priority;
  rtems_interval ticks;
  uint32_t count,max_message_size;
  rtems_attribute attribute_set;

// Set up an additional memory mapping for XLMXXV, geographically 
// addressed in slot 11. It seems to only need 10 MBytes.
/*  printf("Set DBAT 4 for XLM\n");
  setdbat(4,
	  PCI_MEM_BASE + 0xa0000000UL,
	  PCI_MEM_BASE + 0xa0000000UL,
          0x01000000,
          IO_PAGE);
  printf("Set UNIVERSE port for XLM\n");
  BSP_VMEOutboundPortCfg(
	4,  // Port number -  is this the DBAT #
	VME_AM_EXT_USR_DATA | VME_MODE_DBW32,  // VME address modifier
	0x58000000UL,      // VME base address
	0xa0000000UL,      // PCI base address,
	0x01000000UL       // Size of this module
	);
  BSP_VMEOutboundPortsShow(0);

  printf("Finished setting mapping the XLM\n");
*/
// Done Testing  
  rtems_bsdnet_show_mbuf_stats ();

  rtems_bsdnet_initialize_network ();
  rtems_bsdnet_show_inet_routes ();

  rtems_bsdnet_show_mbuf_stats ();

  /*
   * Spawn resetxx task
   */
  my_priority = 5;
  sc = rtems_task_create (rtems_build_name ('r', 'e', 's', 'e'),
       my_priority,
       32*1024,
       RTEMS_PREEMPT|RTEMS_NO_TIMESLICE|RTEMS_NO_ASR|RTEMS_INTERRUPT_LEVEL(0),
       RTEMS_NO_FLOATING_POINT|RTEMS_LOCAL,
       &tidres);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create task; %s\n", rtems_status_text (sc));
    return;
  }
  sc = rtems_task_start (tidres, resetxx, 0);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't start task; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("resetxx started on VME!");
    prettyPrintObjectId(tidres);
  /*
   * Spawn clk100 task
   */
  my_priority = 7;
  sc = rtems_task_create (rtems_build_name ('c', '1', '0', '0'),
       my_priority,
       32*1024,
       RTEMS_PREEMPT|RTEMS_NO_TIMESLICE|RTEMS_NO_ASR|RTEMS_INTERRUPT_LEVEL(0),
       RTEMS_NO_FLOATING_POINT|RTEMS_LOCAL,
       &tidclk);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create task; %s\n", rtems_status_text (sc));
    return;
  }
  sc = rtems_task_start (tidclk, clkxx, 0);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't start task; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("clkxx started on VME!\n");
    prettyPrintObjectId(tidclk);

  /*
   * Spawn devchk task
   */
  rtems_task_set_priority (RTEMS_SELF, RTEMS_CURRENT_PRIORITY, &my_priority);
  sc = rtems_task_create (rtems_build_name ('d', 'e', 'v', 'c'),
       my_priority,
       32*1024,
       RTEMS_PREEMPT|RTEMS_NO_TIMESLICE|RTEMS_NO_ASR|RTEMS_INTERRUPT_LEVEL(0),
       RTEMS_NO_FLOATING_POINT|RTEMS_LOCAL,
       &tiddev);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create task; %s\n", rtems_status_text (sc));
    return;
  }
  sc = rtems_task_start (tiddev, devchk, 0);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't start task; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("devchk started on VME!\n");
    prettyPrintObjectId(tiddev);

/*
*   Give devchk time to finish
*/
   ticks = 50;
   rtems_task_wake_after(ticks);

  /*
   * Spawn cnafxx task
   */
  rtems_task_set_priority (RTEMS_SELF, RTEMS_CURRENT_PRIORITY, &my_priority);
  sc = rtems_task_create (rtems_build_name ('c', 'n', 'a', 'f'),
       my_priority,
       32*1024,
       RTEMS_PREEMPT|RTEMS_NO_TIMESLICE|RTEMS_NO_ASR|RTEMS_INTERRUPT_LEVEL(0),
       RTEMS_NO_FLOATING_POINT|RTEMS_LOCAL,
       &tidcan);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create task; %s\n", rtems_status_text (sc));
    return;
  }
  sc = rtems_task_start (tidcan, cnafxx, 0);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't start task; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("cnafxx started on VME!\n");
    prettyPrintObjectId(tidcan);

  /*
   * Spawn fastxx task
   */
/*
  rtems_task_set_priority (RTEMS_SELF, RTEMS_CURRENT_PRIORITY, &my_priority);
  sc = rtems_task_create (rtems_build_name ('f', 'a', 's', 't'),
       my_priority,
       32*1024,
       RTEMS_PREEMPT|RTEMS_NO_TIMESLICE|RTEMS_NO_ASR|RTEMS_INTERRUPT_LEVEL(0),
       RTEMS_NO_FLOATING_POINT|RTEMS_LOCAL,
       &tidfast);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create task; %s\n", rtems_status_text (sc));
    return;
  }
  sc = rtems_task_start (tidfast, fastxx, 0);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't start task; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("fastxx started on VME!\n");
*/
  /*
   * Spawn vmexx task
   */

  sc = rtems_task_create (rtems_build_name ('v', 'm', 'e', 'x'),
       my_priority,
       32*1024,
       RTEMS_PREEMPT|RTEMS_NO_TIMESLICE|RTEMS_NO_ASR|RTEMS_INTERRUPT_LEVEL(0),
       RTEMS_NO_FLOATING_POINT|RTEMS_LOCAL,
       &tidvme);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create task; %s\n", rtems_status_text (sc));
    return;
  }
  sc = rtems_task_start (tidvme, vmexx, 0);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't start task; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("vmexx started on VME!\n");
    prettyPrintObjectId(tidvme);

  /*
   * Spawn VMEacq task
   */

  sc = rtems_task_create (rtems_build_name ('V', 'a', 'c', 'q'),
       my_priority,
       32*1024,
       RTEMS_PREEMPT|RTEMS_NO_TIMESLICE|RTEMS_NO_ASR|RTEMS_INTERRUPT_LEVEL(0),
       RTEMS_NO_FLOATING_POINT|RTEMS_LOCAL,
       &tidacq);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create task; %s\n", rtems_status_text (sc));
    return;
  }
  sc = rtems_task_start (tidacq, VMEacq, 0);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't start task; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("VMEacq started on VME!\n");
    prettyPrintObjectId(tidacq);

  /*
   * Spawn memmgrxx task
   */

  sc = rtems_task_create (rtems_build_name ('m', 'g', 'r', 'x'),
       my_priority,
       32*1024,
       RTEMS_PREEMPT|RTEMS_NO_TIMESLICE|RTEMS_NO_ASR|RTEMS_INTERRUPT_LEVEL(0),
       RTEMS_NO_FLOATING_POINT|RTEMS_LOCAL,
       &tidmem);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create task; %s\n", rtems_status_text (sc));
    return;
  }
  sc = rtems_task_start (tidmem, memmgrxx, 0);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't start task; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("memmgrxx started on VME!\n");
    prettyPrintObjectId(tidmem);
/*
 *   Create message queue
 */
  
  count = 2;
  max_message_size = 4;
  attribute_set = RTEMS_FIFO | RTEMS_LOCAL;
  sc = rtems_message_queue_create (rtems_build_name ('A', 'c', 'q', '1'),
       count,
       max_message_size,
       attribute_set,
       &Que1id);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create queue; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("Acq1 queue created\n");
    prettyPrintObjectId(Que1id);
  
  /*
   * Spawn data_proc task
   */

  sc = rtems_task_create (rtems_build_name ('p', 'r', 'o', 'c'),
       my_priority,
       32*1024,
       RTEMS_PREEMPT|RTEMS_NO_TIMESLICE|RTEMS_NO_ASR|RTEMS_INTERRUPT_LEVEL(0),
       RTEMS_NO_FLOATING_POINT|RTEMS_LOCAL,
       &tidproc);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create task; %s\n", rtems_status_text (sc));
    return;
  }
  sc = rtems_task_start (tidproc, data_proc, 0);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't start task; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("data_proc started on VME!\n");
    prettyPrintObjectId(tidproc);


  /*
   * Spawn vmemon task
   */

  sc = rtems_task_create (rtems_build_name ('V', 'm', 'o', 'n'),
       my_priority,
       32*1024,
       RTEMS_PREEMPT|RTEMS_NO_TIMESLICE|RTEMS_NO_ASR|RTEMS_INTERRUPT_LEVEL(0),
       RTEMS_NO_FLOATING_POINT|RTEMS_LOCAL,
       &tidmon);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't create task; %s\n", rtems_status_text (sc));
    return;
  }
  sc = rtems_task_start (tidmon, vmemon, 0);
  if (sc != RTEMS_SUCCESSFUL) {
    printf ("Can't start task; %s\n", rtems_status_text (sc));
    return;
  }
  else
    printf("vmemon started on VME!\n");
    prettyPrintObjectId(tidmon);


 /*
   * Wait for characters from console terminal
   */
  int i;
  for (i=0;i<10;i++)
    printf("debugdata init: %lx\n", debugdata[i]);

/*   Print an initial help message  */
      printf("This is vmeacq.ralf - Version of 16 November 2017.  \n");
      printf("Syntax:\n");
      printf("q - reboot the processor\n");
      printf("s - show network statistics \n");
      printf("d - print debugging array \n");
      printf("z - zero debugging array \n");

  for (;;) {
    switch (getchar ()) {
    case 'q':
      bsp_reset();
      break;
    case 's':
      showStatistics ();
      break;
    case 'd':
      debugprint();
      break;
    case 'z':
      debugzero();
      break;
    default:
      printf("Syntax:\n");
      printf("q - reboot the processor\n");
      printf("s - show network statistics \n");
      printf("d - print debugging array \n");
      printf("z - zero debugging array \n");
      break;
    }
  }
  
  exit(0);
}
