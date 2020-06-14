#ifndef  TCB_H_
#define  TCB_H_

#define MAXARG     10               /* max argument count of the cmd line */
#define MAXBP      10               /* max 10 breakpoints                 */
#define MAXNAME     5               /* max 5 names in name buffer         */
#define TMAX       64               /* Max number of tasks                */
#define ARGLEN     20               /* maximum argument length            */

/*  special system flags for VMEPROM                                      */

#define SOMEREG 0x0001              /* display only PC, A7, A6, A5        */
#define T_DISP  0x0002              /* no register display during trace (TC>1) */
#define T_SUB   0x0004              /* trace over subroutine set          */
#define T_ASUB  0x0008              /* trace over subroutine active       */
#define T_RANG  0x0010              /* trace over range set               */
#define REG_INI 0x0020              /* no register initialization if set  */
#define RE_DIR  0x0040              /* output redirection to file and     */
                                    /* console at same time               */

#define BYTE    unsigned char
#define WORD    unsigned short
#define LWORD   unsigned int

struct TCB {
  char _ubuf[256];                  /* 256 byte user buffer               */
  char _clb[80];                    /* 80 byte monitor command line buffer */
  char _mwb[32];                    /* 32 byte monitor parameter buffer   */
  char _mpb[60];                    /* monitor parameter buffer           */
  char _cob[8];                     /* character output buffer            */
  char _swb[508];                   /* system work buffer/task pdos stack */
  char *_tsp;                       /* task stack pointer                 */
  char *_kil;                       /* kill self pointr                   */
  long _sfp;
  char _svf;                        /* save flag FP processor             */
  char _iff;
  long _trp[16];                    /* user TRAP vectors                  */
  long _zdv;                        /* zero divide trap                   */
  long _chk;                        /* CHCK instruction trap              */
  long _trv;                        /* TRAPV instruction trap             */
  long _trc;                        /* trace vector                       */
  long _fpa[2];                     /* floating point accumulator         */
  long *_fpe;                       /* FP error processor address         */
  char *_clp;                       /* command line pointer               */
  char *_bum;                       /* beginning of user memory           */
  char *_eum;                       /* end of user memory                 */
  char *_ead;                       /* entry address                      */
  char *_imp;                       /* internal memory pointer            */
  short _aci;                       /* assigned input file ID             */
  short _aci2;                      /* assigned input file ID's           */
  short _len;                       /* last error number                  */
  short _sfi;                       /* spool file ID                      */
  BYTE _flg;                        /* task flags (bit 8=command line echo */
  BYTE _slv;                        /* directory level                    */
  char _fec;                        /* file expansion count               */
  char _spare1;
  char _csc[2];                     /* clear screen escape seq            */
  char _psc[2];                     /* position cursor                    */
  char _sds[3];                     /* alternate system disks             */
  BYTE _sdk;                        /* system disk                        */
  char *_ext;                       /* XEXT address                       */
  char *_err;                       /* XERR address                       */
  char _cmd;                        /* command line delimiter             */
  BYTE _tid;                        /* task id                            */
  char _ecf;                        /* echo flag                          */
  char _cnt;                        /* output column counter              */
  char _mmf;                        /* memory modified flag               */
  char _prt;                        /* input port #                       */
  char _spu;                        /* spool unit mask                    */
  BYTE _unt;                        /* output unit mask                   */
  char _u1p;                        /* unit 1 port #                      */
  char _u2p;                        /* unit 2 port #                      */
  char _u4p;                        /* unit 4 port #                      */
  char _u8p;                        /* unit 8 port #                      */
/***************************************************************************
*   CAUTION:  The following two elements were added at ORNL.
*    The size of _spare2 was decreased to account for these.  Future
*    revisions of VMEPROM may conflict with these additions.  BEWARE!!
*    The variables are used by system.c for heap allocation.  The code
*    mem_mgr sets both variables when a task is started.
*
*    _brk  -  First address available for allocation to heap use.
*
*    _brk_size - Number of bytes available starting at _brk
*
***************************************************************************/
  char *_brk;                       /* Highest memory loaded              */
  long _brk_size;                   /* Heap size                          */

  char _spare2[18];
} ;

#endif         /* end  TCB_H_   */
