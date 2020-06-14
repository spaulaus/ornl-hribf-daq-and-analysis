/*
*   Module types
*/

#define   PHIL   1
#define   SILENA   2
#define   LRS_4300   3
#define   HHIRF_ADC   4
#define   LRS_4508   5
#define   LRS_4418   6
#define   UE_CLOCK   7
#define   PHIL_7106   8
#define   LRS_4415A   9
#define   LRS_3511   10
#define   LRS_4413   11
#define   LRS_4416B   12
#define   LRS_3377   13
#define   SHAPER   14
#define   CAEN_775   15
#define   CAEN_785   16
#define   CAEN_792   17
#define   MCSQ   18

/*
*   Module data structure
*/
      struct modu_data {
         int mt_type;   /* PHIL, SILENA etc.                                 */
         int mt_line;   /* line number                                       */
         int mt_error;  /* 0 means OK, nonzero means error                   */
         int c;         /* Crate number                                      */
         int n;         /* Slot number                                       */
         int low_set;   /* nonzero means lower_thresholds were set           */
         int low[32];   /* lower_thresholds                                  */
         int up_set;    /* nonzero means upper_thresholds were set           */
         int up[16];    /* upper_thresholds                                  */
         int ped_set;   /* nonzero means pedestals were set                  */
         int ped[16];   /* pedestals/gains                                   */
         int off_set;   /* nonzero means offsets were set                    */
         int off[16];   /* offsets                                           */
         int com_set;   /* nonzero means common_threshold was set            */
         int com;       /* common_threshold                                  */
         int enable_set;/* nonzero means enables were set                    */
         int enable[16];/* channel enables                                   */
         int gen_set;   /* nonzero means generate function was specified     */
         int gen_type;  /* generate function type                            */
         int delay_set; /* nonzero means delays were specified               */
         int delay[16]; /* delays                                            */
         int mode_set;  /* nonzero means mode was set                        */
         int mode;      /* mode word                                         */
         int ovfl_set;  /* nonzero means overflow was set                    */
         int ovfl;      /* overflow flag                                     */
      };

/*
*   Data types
*/

#define   LOW   0
#define   UP   1
#define   OFF   2
#define   PED   3
#define   COM   4
#define   ENABLE   5
#define   GE_ONLY   6
#define   BGO_ONLY   7
#define   ANTI   8
#define   DELAY   9
#define   MODE   10
#define   OVFL   11
#define   GAINS   3

/*
*  Error codes
*/

#define   READ_EOF   -2      /* end_of_file                                  */
#define   READ_MT   -1       /* module type specification                    */
#define   ERR_NUM   1 
#define   ERR_C_VAL   2      /* invalid CAMAC crate number                   */
#define   ERR_N_VAL   3      /* invalid CAMAC slot number                    */
#define   ERR_2N   4         /* second CAMAC slot less than first            */
#define   ERR_CONV   5       /* number conversion error                      */
#define   ERR_MOD_UNKN   6   /* unknown module type                          */
#define   ERR_MOD_AMBIG   7  /* ambiguous module specification               */
#define   ERR_DAT_UNKN   8   /* unknown data type                            */
#define   ERR_DAT_AMBIG   9  /* ambiguous data type specification            */
#define   ERR_NAN   10       /* not a numeric field                          */
#define   ERR_DATA_TYPE   11 /* invalid data type for this module            */
#define   ERR_DATA_VAL   12  /* data value out of range                      */
#define   ERR_DUP_DATA   13  /* duplicate data for this parameter            */
#define   ERR_TOO_FEW   14   /* too few data                                 */
#define   ERR_PRIOR_MOD_TYPE   15  /* prior module assign to c and n         */
#define   ERR_PRIOR_LOW   16  /* prior def of lower threshold                */
#define   ERR_PRIOR_UP   17   /* prior def of upper threshold                */
#define   ERR_PRIOR_OFF   18  /* prior def of offset memory                  */
#define   ERR_PRIOR_PED   19  /* prior def of pedestals                      */
#define   ERR_PRIOR_COM   20  /* prior def of common threshold               */
#define   ERR_PRIOR_ENABLE   21  /* prior def of enables                     */
#define   ERR_PRIOR_GEN   22  /* prior def of generate function              */
#define   ERR_PRIOR_DELAY   23   /* prior def of delays                      */
#define   ERR_MULTI_C   24    /* Multiple Crate specs                        */
#define   ERR_MULTI_N   25    /* Multiple Slot specs                         */
#define   ERR_NO_N   26       /* No Crate spec                               */
#define   ERR_NO_C   27       /* No Slot spec                                */
#define   ERR_TOO_MANY   28   /* Too many data                               */
#define   ERR_UNKN_KEY   29   /* Unknown key word                            */
#define   ERR_DATA_ILL   30   /* Illegal value                               */
#define   ERR_READ_MT   31    /* Error searching for mt                      */
