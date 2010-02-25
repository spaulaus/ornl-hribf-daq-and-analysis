/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1992-1995
*
*
*
*                         IMPORTANT NOTICE TO USER
*
*     All statements, technical information and recommendations contained
*     herein are based on tests we believe to be reliable, but the accuracy
*     or completeness thereof is not guaranteed, and the following  is
*     made in lieu of all warranties expressed or implied:
*
*     Our only obligation shall be to correct or replace, at our convenience,
*     any part of the product proven to be defective.  We shall not be liable
*     for any loss or damage, direct or consequential, arrising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  Force Computers CPU-40/A VME 68040 processor
*
*    File:         /usr/users/mcsq/Dvme3/vmeprom.h
*
*    Description:  C prototypes for functions in vmeprom.s.  All of these
*                  routines are in assembly language.  Most simply pass
*                  calling parameters to functions in VMEPROM provided
*                  with the FORCE Computers CPU-40/A.  A few other assembly
*                  routines are included here because they are used so often.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    2/15/92    MCSQ         
*
*    7/29/92    MCSQ       add  set_intr_level_
*
*   10/13/92    MCSQ       add  cache_inval_
*
*    3/18/95    MCSQ       Corrected type of second argument in wait_evts_
*****************************************************************************/
#ifndef  VMEPROM_H_
#define  VMEPROM_H_

/*
*    Functions for handling Event flags
*/
int  clr_evt_(unsigned char event);
void delay_(unsigned long ticks);
int  dly_clr_evt_(unsigned char event,unsigned long time);
int  dly_set_evt_(unsigned char event,unsigned long time);
void log_phys_(unsigned char event0,unsigned char event1,
                        char **ad0,char **ad1,int *desc);
int  set_evt_(unsigned char event); 
int  set_phys_(int desc,char *addr); 
int  test_evt_(int event);
int  wait_evts_(unsigned char event1, unsigned char event2);
int  wait_phys_(int desc,char *ad0,char *ad1);

/*
*    Message and message pointer routines
*/
int  receive_ptr_(int slot,int *task,char **msg);
int  send_ptr_(int slot,char *msg);
int  send_msg_(int task,char *msg);

/*
*    Memory allocation
*/
void cache_inval_(unsigned char,void *,void *);
char *mem_alloc_low_(int size);
char *mem_alloc_high_(int size);
int  free_mem_(int size,char *addr);
int  get_mem_(int size,char **start,char **end);
void mem_lim_(char **prog,char **eom,char **last,char **syram,char **tcb);
void move_stack_(char *new_top,char *old_top);

/*
*   Swap bytes and word since our host is a Little Endian machine
*/
void byte_swap_(void *buf, int count);
int  word_swap_(void *buf, int count);

/*
*   Task control and status functions
*/
int  spawn_task_(int size,int pri,int port,
                         char *low_mem,char *high_mem,char*start);
void task_exit_(void);
void task_kill_(int task);
int  task_status_(int task);
void task_swap_(void);
int  task_priority_(int task,int time,int priorty);
int  lock_(void);
void unlock_(void);
void super_mode_(void);
void user_mode_(void);
int  set_intr_level_(int);

/*
*   Console I/O routines
*/
int  INPUT(void);
void OUTPUT(char);
int  get_line_(void **);

/*
*   Time and date functions
*/
char *get_time_(void);
char *get_date_(void);

#endif            /* end   VMEPROM_H_     */
