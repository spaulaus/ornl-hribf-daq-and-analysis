/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 2015
*
*
*
******************************************************************************
*
*    Environment:  VME based Data Acquisition System
*
*    File:         acq2/include/myriad.h
*
*    Description:  Include file for ANL/Gammasphere/Gretina MyRIAD module
******************************************************************************
*
*    Revision History:
*    Date       Programmer   Comments
*    7/31/2015     RLV         
*
*****************************************************************************/

#ifndef  MYRIAD_H_
#define  MYRIAD_H_

/* This structure should reflect the register structure of the MyRIAD board.
 * If the documentation is right....
 * If I can count correctly....
 * The char dumNNN arrays reflect the gaps in the register placement.
*/
struct MyRIAD_Registers {
       unsigned int board_id; /* 0x0000 */
       unsigned char dum1[2];
       unsigned int fifo_status; /* 0x0004 */
       unsigned char dum2[26];
       unsigned int hardware_status; /* 0x0020 */
       unsigned char dum3[1002];
       unsigned int pulsed_control; /* 0x040C */
       unsigned int fifo_control;   /* 0x040E */
       unsigned int capture_time;   /* 0x0410 */
       unsigned char dum4[494];
       unsigned int code_revision;  /* 0x0600 */
       unsigned char dum5[4];
       unsigned int code_date;      /* 0x0604 */
       unsigned int code_year;      /* 0x0606 */
       unsigned char dum6[248];
       unsigned int nim_status;            /* 0x0700 */
       unsigned int gating_register;       /* 0x0702 */
       unsigned int ecl_status_A;          /* 0x0704 */
       unsigned int ecl_status_B;          /* 0x0706 */
       unsigned int latched_timestamp_A;   /* 0x0708 */
       unsigned int latched_timestamp_B;   /* 0x070A */
       unsigned int latched_timestamp_C;   /* 0x070C */
       unsigned int serdes_command_format; /* 0x070E */
       unsigned int aux_detector_trigger_delay; /* 0x0710 */
       unsigned int gs_trig_gate;          /* 0x0712 */
       unsigned int system_timestamp_A;    /* 0x0714 */
       unsigned int system_timestamp_B;    /* 0x0716 */
       unsigned int system_timestamp_C;    /* 0x0718 */
       unsigned int ts_err_cntr_ctrl_A;    /* 0x071A */
       unsigned int ts_err_cntr_ctrl_B;    /* 0x071C */
       unsigned int timestamp_error_count; /* 0x071E */
       unsigned char dum7[2];
       unsigned int ttcl_time_offset;      /* 0x0722 */
       unsigned int missed_trig_count;     /* 0x0724 */
       unsigned int delayed_trig_error_count; /* 0x0726 */
       unsigned int propagation_control;   /* 0x0728 */
       unsigned char dum8[188];
       unsigned int fifo_counter;          /* 0x07EC */
       unsigned char dum9[4];
       unsigned int trig_counter;          /* 0x07F0 */
       unsigned int user_counter_0;        /* 0x07F2 */
       unsigned int user_counter_1;        /* 0x07F4 */
       unsigned int user_counter_2;        /* 0x07F6 */
       unsigned int user_counter_3;        /* 0x07F8 */
       unsigned int user_counter_4;        /* 0x07FA */
       unsigned int user_counter_5;        /* 0x07FC */
       unsigned int user_counter_6;        /* 0x07FE */
       unsigned int user_counter_7;        /* 0x0800 */
       unsigned char dum10[70];
       unsigned int serdes_config;         /* 0x0848 */
       unsigned char dum11[182];
       unsigned int vme_fpga_control;      /* 0x0900 */
       unsigned int vme_fpga_status;       /* 0x0902 */
       unsigned int vme_config_control;    /* 0x0906 */
       unsigned int vme_flash_vpen;        /* 0x0908 */
       unsigned int vme_config_start_low;  /* 0x090A */
       unsigned int vme_config_start_high; /* 0x090C */
       unsigned int vme_config_stop_low;   /* 0x090E */
       unsigned int vme_config_stop_high;  /* 0x0910 */
       unsigned char dum12[6];
       unsigned int vme_sandbox_A; /* 0x0918 */
       unsigned int vme_sandbox_B; /* 0x091A */
       unsigned int vme_sandbox_C; /* 0x091C */
       unsigned int vme_sandbox_D; /* 0x091E */
       unsigned char dum13[96];
       unsigned int vme_flash_access_address_low;  /* 0x0980 */
       unsigned int vme_flash_access_address_high; /* 0x0982 */
       unsigned int vme_flash_access_data;         /* 0x0984 */
       unsigned int vme_flash_access_data_auto;    /* 0x0986 */

       unsigned char dum14[1656];

       unsigned int fifo_access;           /* 0x1000 */
       
       
};

#endif      /* end  MYRIAD_H_   */
