/*****************************************************************************
*
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1989-1995
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
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME based data acquisition system
*
*    File:         /usr/users/mcsq/Dvme3/vmedis.c
*
*    Description:
*                 This is a C version of the finest MC68020/MC68882
*                 disassembler available at HHIRF on 3 September 1989.
*                 This is a version of DIS040 for use with the CPU-40
*                 in the VME data acquisition system.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    9/ 3/89    MCSQ         Original
*
*    2/28/90    MCSQ         Fix detection of word and long word access at
*                            odd byte address for MC68000 and MC68010.
*
*    9/27/91    MCSQ         Source and destination operands for the 
*                            EOR  Dn,<ea>  instruction were interchanged.
*                            (i.e. it was EOR  <ea>,Dn ).
*
*    2/15/92    MCSQ         Added minimal support for MC68040.
*
*    6/15/92    MCSQ         Added full support for MC68030 and MC68040.
*                            Not included are EC versions or MC683XX controllers
*
*   11/10/92    MCSQ         Adapted for use in debugging the Force CPU-40.
*                            Removed command and file input routines and
*                            made may a function called from a CPU-40
*                            debug routine.
*
*    2/18/94    MCSQ         Replaced reference to malloc.h with stdlib.h
*
*    2/18/95    MCSQ         Corrected error in ea_code for address modes
*                            ([Xn]) and ([bd,Xn]).   Also corrected decode
*                            of FTST in fpgen.
*
*    3/ 6/97    MCSQ         In ea_code routine, allow the full extension
*                            word format with Index Register Type set to An
*                            when Index Suppress is set to 1.
*****************************************************************************/

/*****************************************************************************
*   Global variable definitions for DIS040.
*
*   Disassembles code for Motorola MC68000, MC68010, MC68020, MC68030 and
*   MC68040 processors.  Optionally, an MC68881/MC68882 floating-point 
*   coprocessor can be used with a MC68020 or MC68030.
*
******************************************************************************/


int cpu = 4;                /* cpu type: 0 = 68000, 1 = 68010, 2 = 68020
                                     3 = 68030, 4 = 68040             */
int fpcp = 1;               /* nonzero = 68881/68882 coprocessor present  */

#define REF_MAX 8192
int ref_end = -1;      /* index of end of reference table */
int ref_pc;            /* index of last label found       */
int ref_last = -1;     /* index of end of table before this
			  instruction */
int ref_ovfl = 0;

unsigned long int ref_list[REF_MAX];   /* reference list */

unsigned char *hex_ptr;        /* pointer to source hexadecimal array */
unsigned char *hex_array;      /* source array   */

char instr_str[12];     /* instruction nemonic  */
char oprand_str[70];    /* operand string       */
unsigned long int pc;   /* program counter      */
long int ir_len;        /* instruction length   */
char instr_valid;       /* TRUE for valid instruction */
char warn;              /* TRUE if assembliers may use different code */

unsigned long int org;    /* program counter orgin of source array */
unsigned long int pcend;  /* program counter end of source array   */

char warn_enab;         /* TRUE if warning are listed */

long int instr_wd;      /* first word of an instruction  */
int op_class;           /* most significant 4 bits of the instruction word */
int op_cc;              /* condition code field (4 bits) */
int op_size;            /* operation size code           */
int dest_m;             /* destination mode field        */
int dest_r;             /* destination register field    */
int ea_m;               /* effective address mode field  */
int ea_r;               /* effective address register field */
unsigned long int op_value;  /* operand value             */
char op_addr;           /* TRUE if op_value is an address */
char op_imm;            /* TRUE if op_value is immediate data */


enum mode {DATA,MEM,CNTL,ALTER,DATA_ALTER,MEM_ALTER,CNTL_ALTER } mode;

char *size_codes[3] = {".B","",".L"};
char *cc_codes[16] = {"T","F","HI","LS","CC","CS","NE","EQ",
                    "VC","VS","PL","MI","GE","LT","GT","LE"};
char *reg_codes[17] = {"D0","D1","D2","D3","D4","D5","D6","D7",
		     "A0","A1","A2","A3","A4","A5","A6","SP","PC"};
char *fpsize_codes[8] = {".L",".S",".X",".P",".W",".D",".B",".P"};
char *fpreg_codes[8] = {"FP0","FP1","FP2","FP3","FP4","FP5","FP6","FP7"};
char *fpcc_codes[32] = {"F","EQ","OGT","OGE","OLT","OLE","OGL","OR",
			"UN","UEQ","UGT","UGE","ULT","ULE","NE","T",
			"SF","SEQ","GT","GE","LT","LE","GL","GLE",
			"NGLE","NGL","NLE","NLT","NGE","NGT","SNE","ST"};

/*
*    function prototypes
*/

void addr_modes(enum mode);
void ascii_code(char *);
unsigned char ascii_hex(char *,int *);
void class_0(void);
void class_1(void);
void class_2(void);
void class_3(void);
void class_4(void);
void class_5(void);
void class_6(void);
void class_7(void);
void class_8(void);
void class_9(void);
void class_10(void);
void class_11(void);
void class_12(void);
void class_13(void);
void class_14(void);
void class_15(void);
void dcb_code(unsigned long int,char *);
void dcl_code(unsigned long int,char *);
void dcw_code(unsigned long int,char *);
long int disp_code(int,char,char *);
void rel_code(int,unsigned long,char *);
void ea_code(char *);
void mmu30(char *,char *);
void mmu40(char *,char *);
void move16(char *,char *);
void fpbcc(char *,char *);
void fpdbcc(char *,char *);
void fpgen(char *,char *);
void fpreg_list(int ,char *);
void instr_finis(char *,char *);
void instr_parse(void);
long int get_dat(int);
long int imm_code(char *);
void num_code(long int,char *);
void order(void);
void params(void);
void readit(void);
void ref_bld(void);
void reg_list(char *);
void rxry_code(int,char *,char *);

#define TRUE 1
#define FALSE 0
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


unsigned char *vmedis(int pass,unsigned char *pcstart,unsigned char *pcstop,
                                                 int space_type,int line_count)
{
   int   i,out_len,lc;
   char  tmp_str[12];
   unsigned char *hptr;
   unsigned char *optr;
   unsigned char  out_line[141];
   unsigned long int count,tstpc;

   if (pass == 1)
    {
      ref_last = -1;
      ref_end  = -1;
      org = (unsigned long)pcstart;
    }
   ref_pc  = 0;
   hex_ptr = pcstart;
   pc = (unsigned long)pcstart;
   if (pcstop == 0) return(0);
   pcend = (unsigned long)pcstop;
   lc = line_count;
/*
*   This is the start of the main loop which does one line of output
*   per pass.
*/
NEXT:
   ir_len = 0;
   instr_valid = TRUE;
   warn = FALSE;
   count = pcend - pc;
   if (pass == 2)
     {
       while (ref_list[ref_pc] < pc && ref_pc < ref_end)
	 ref_pc += 1;
/*
*   Check for labels within blocks decoded as byte, word or long.
*   If any, adjust block count to stop at next label.
*/
       if (space_type <= 2 && ref_end >= 0)
	 {
	   tstpc = ref_list[ref_pc];
	   if (ref_list[ref_pc] == pc && ref_pc < ref_end) tstpc
							= ref_list[ref_pc+1];
	   if (tstpc > pcend) tstpc = pcend;
	   if (tstpc < pcend) count = tstpc - pc;
	 }
     }
/*
*   Dispatch on the type of space to decode
*/
   if (space_type == 0)
     {
/*
*   type  = BYTE
*/
       strcpy(instr_str,"DC.B");
       dcb_code(count,oprand_str);
     }
   else if (( pc & 1) != 0)
     {
/*
*   BYTE types can leave PC at an odd address.  If so, we must make
*   the pc even for other types(in particular CODE);
*/
       strcpy(instr_str,"DC.B");
       dcb_code(1,oprand_str);
     }
   else if (space_type == 1)
     {
/*
*   type  =  WORD
*/
       strcpy(instr_str,"DC.W");
       dcw_code(count,oprand_str);
     }
   else if (space_type == 2)
     {
/*
*   type  =  LONG
*/
       strcpy(instr_str,"DC.L");
       dcl_code(count,oprand_str);
     }
   else if (space_type == 5)
     {
/*
*   type  = ADDR.W (i.e. 16-bit address table
*/
       strcpy(instr_str,"DC.W");
       disp_code(9,0,oprand_str);
     }
   else if (space_type == 6)
/*
*   type  = ADDR.L (i.e. 32-bit address table
*/
     {
       strcpy(instr_str,"DC.L");
       disp_code(10,0,oprand_str);
     }
   else
/*
*    None of the above. So do CODE.
*/
      instr_parse();
   if (pass == 1)
/*
*   On pass 1, build a table of label references.
*/
      ref_bld();
   else
     {
/*
*   On pass 2, format and output line to the list file
*/
       ref_end = ref_last;
       memset(out_line,' ',120);
       out_line[120] = '\0';
       if (pc == ref_list[ref_pc])
         {
           out_line[0] = 'L';
           num_code(pc,tmp_str);
           if (pc < 0x10000)
              memcpy(&out_line[1],&tmp_str[4],4);
           else
	      memcpy(&out_line[1],tmp_str,8);
           if (ref_pc < ref_end) ref_pc += 1;
	 }
       memcpy(&out_line[10],instr_str,strlen(instr_str));
       memcpy(&out_line[22],oprand_str,strlen(oprand_str));
       out_len = 22 + strlen(oprand_str);
       if (out_len < 44) out_len = 43;
       strncpy((char *)&out_line[out_len]," !PC",4);
       num_code(pc,tmp_str);
       strncpy((char *)&out_line[out_len+4],tmp_str,8);
       out_len = out_len + 12;
       optr = &out_line[out_len];
       if (space_type == 0 || space_type == 3
                                    || space_type > 6)
         {
	   i = 0;
           hptr = hex_ptr;
	   while (i < (int) ir_len)
             {
               if (i % 2 == 0) ++optr;
	       *optr = ((*hptr >> 4) &0xF) + 0x30;
               if (*optr > 0x39) *optr += 7;
               ++optr;
               *optr = (*hptr++ & 0xF) + 0x30;
               if (*optr > 0x39) *optr += 7;
               ++optr;
               i += 1;
             }
	   out_len = out_len + optr - &out_line[out_len];
	 }
       if (space_type >= 1 && space_type <= 3)
         {
	   if (out_len < 70) out_len = 70;
	   ascii_code((char *)&out_line[out_len+3]);
	 }
       else
	 *optr = '\0';
       lan_printf("%s\n",out_line);
       if (strncmp(instr_str,"JMP",3) == 0 || strncmp(instr_str,"BRA",3) == 0
	   || strncmp(instr_str,"RT",2) == 0) {lan_printf("*\n"); --lc;}
       if (--lc <= 0) return((unsigned char *)(pc+ir_len));
     }
   pc = pc + ir_len;
   hex_ptr = hex_ptr + ir_len;
   if (pc >= pcend)
     {
PASS_END:;
       if (pass == 1)
        {
          i = 0;
          order();
        }
       return((unsigned char *)pc);
   
     }
   goto NEXT;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$   ascii_code    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Encode instruction byte into ASCII characters for listing.  Non-printing
*   characters are converted to periods.
*/

void ascii_code (char *str)
{
   char  xchar;
   unsigned char  *ptr;

   ptr = hex_ptr;
   while (ptr < hex_ptr + ir_len)
     {
       if (*ptr < ' ' || *ptr == 0x7b || *ptr == 0x7d || *ptr >= 0x7f)
           xchar = 0x2e;
       else
	   xchar = *ptr;
       *str++ = xchar;
       ptr++;
     }
   *str = '\0';
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     ascii_hex   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Convert ASCII hex to a byte
*/
unsigned char ascii_hex(char *str,int *err)
{
   int  nibble;
   unsigned char byte;

   nibble = *str++ - 0x30;
   if (nibble < 0) *err = 1;
   if (nibble > 9) nibble -= 7;
   byte = nibble << 4;
   nibble = *str++ - 0x30;
   if (nibble < 0) *err = 1;
   if (nibble > 9) nibble -= 7;
   return (byte + nibble);
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$  instr_parse $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Routine gets the next 16-bit word from the source array and decodes
*   it into the various bit fields used to decode the intruction.
*   The routine dispatchs to one of 16 subroutine one the basis of the
*   4 most significant bits (op_class) of the 16-bit word.
*/

void instr_parse (void)
{
    op_addr = FALSE;
    op_imm = FALSE;
    instr_wd = get_dat(2);
    op_class = (int) ((instr_wd >> 12) & 0XF);
    op_cc    = (int) ((instr_wd >> 8 ) & 0XF);
    op_size  = (int) ((instr_wd >> 6 ) & 0X3);
    dest_r   = (int) ((instr_wd >> 9 ) & 0X7);
    dest_m   = (int) ((instr_wd >> 6 ) & 0X7);
    ea_m     = (int) ((instr_wd >> 3 ) & 0X7);
    ea_r     = (int) (instr_wd & 0X7);
    switch (op_class)
    {
      case 0:
	class_0();
	break;
      case 1:
	class_1();
        break;
      case 2:
	class_2();
        break;
      case 3:
	class_3();
        break;
      case 4:
        class_4();
        break;
      case 5:
	class_5();
	break;
      case 6:
	class_6();
	break;
      case 7:
	class_7();
	break;
      case 8:
	class_8();
	break;
      case 9:
	class_9();
	break;
      case 10:
	class_10();
	break;
      case 11:
	class_11();
	break;
      case 12:
	class_12();
	break;
      case 13:
	class_13();
	break;
      case 14:
	class_14();
	break;
      case 15:
        class_15();
	break;
      default:
        break;
    }
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$   instr_finis   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Finish processing an instruction.  If instr_valid is TRUE, combine the
*   sorc_str and dest_str into oprand_str.  Oherwise, set instr_str to
*   DC.W, set ir_len to 2 and encode one 16-bit word as ASCII hex.
*/

void instr_finis (char *sorc_str,char *dest_str)
{
   if (cpu < 2 && op_size > 0)
     if (op_addr == TRUE && (op_value & 1) != 0) instr_valid = FALSE;
   if (instr_valid == TRUE)
   {
     strcpy(oprand_str,sorc_str);
     if (strlen(dest_str) != 0)
     {
       strcat(oprand_str,",");
       strcat(oprand_str,dest_str);
     }
   }
   else
   {
     warn = FALSE;
     strcpy(instr_str,"DC.W");
     strcpy(oprand_str,"$");
     num_code(instr_wd,sorc_str);
     strcat(oprand_str,sorc_str + 4);
     ref_end = ref_last;
     ir_len = 2;
   }
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$    ea_code   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Evaluate an effective address code.
*/

void ea_code(char *str)
{
   static char *scale_codes[4] = {"","*2","*4","*8"};
   int         bs,is,iis,abs_size,bd_size,dsp_size;
   int         reg_num;
   long int    scale,temp;
   char        tmp_str[36];
   char        reg_str[12],od_str[12],xreg_str[12],base_str[12];

   *str = '\0';
/*
*   Check for illegal PC modes and An direct with byte size
*/
   if (ea_m == 7 && ea_r > 4) instr_valid = FALSE;
   if (ea_m == 1 && op_size == 0) instr_valid = FALSE;
   if (!instr_valid) return;

   op_addr = FALSE;
   op_imm  = FALSE;
/*
*   Encode the register part of the effective address
*/
   reg_num = ea_r;
   if (ea_m == 7)
      reg_num = 16;
   else 
      if (ea_m != 0) reg_num = reg_num + 8;
   strcpy(reg_str,"(");
   strcat(reg_str,*(reg_codes + reg_num));
   strcat(reg_str,")");
   if (ea_m <= 1)
/*
*   Data or Address register direct
*/
      strcpy(str,*(reg_codes + reg_num));
   else if (ea_m == 2)
/*
*   Address register indirect
*/
	strcpy(str,reg_str);
   else if (ea_m == 3)
/*
*   Address register indirect with postincrement
*/
      {
         strcpy(str,reg_str);
         strcat(str,"+");
      }
   else if (ea_m == 4)
/*
*   Address register indirect with predecrement
*/
      {
	 strcpy(str,"-");
         strcat(str,reg_str);
      }
   else if (ea_m == 5)
      {
         disp_code(5,0,str);
         strcat(str,reg_str);
      }
/*
*   Address register or PC indirect with index and displacement and
*   memory indexed.
*/
   else if (ea_m == 6 || (ea_m == 7 && ea_r == 3))
      {
         temp = get_dat(2);
	 strcpy(xreg_str,*(reg_codes + ((temp >> 12) & 0xF)));
         if ((temp & 0x800) == 0)
            strcat(xreg_str,".W");
         else
            strcat(xreg_str,".L");
/*
*   Append scale code to the index register string - MC68020 only
*/
	 scale = (temp >> 9) & 3;
	 strcat(xreg_str,*(scale_codes + scale));
         if ((temp & 0x100) == 0)
           {
/*
*   Brief format extension word
*/
	     temp = temp & 0xFF;
             if ((temp & 0x80) != 0) temp = temp | 0xFFFFFF00;
	     dsp_size = 4;
/*
*   Check for PC relative mode
*/
             if (ea_m == 7) dsp_size = 0;
	     disp_code(dsp_size,(char) temp,tmp_str);
             if (scale == 0)
                {
                  strcpy(str,tmp_str);
                  strncat(str,reg_str,3);
                  strcat(str,",");
                  strcat(str,xreg_str);
                  strcat(str,")");
		}
             else
                {
                  if (cpu < 2) instr_valid = FALSE;
                  strcpy(str,"(");
                  strcat(str,tmp_str);
                  strcat(str,",");
		  strncat(str,reg_str + 1,2);
                  strcat(str,",");
                  strcat(str,xreg_str);
                  strcat(str,")");
		}
	   }
          else
/*
*   Full format extension word(s) - MC68020 up
*/
           {
                if (cpu < 2) instr_valid = FALSE;
                bs = 0;
                is = 0;
		bd_size = (int) ((temp >> 4) & 3);
		iis = (int) (temp & 7);
                if ((temp & 0x80) != 0)
                  {
                    bs = 1;
                    if (ea_r != 0) instr_valid = FALSE;
                  }
                if ((temp & 0x40) != 0)
                  {
                    is = 1;
                    if ((temp & 0x7E00) != 0) instr_valid = FALSE;
                    if (iis >= 4) instr_valid = FALSE;
                  }
                if ((temp & 0x8) != 0) instr_valid = FALSE;
                if (bd_size == 0) instr_valid = FALSE;
/*
*   Get Base displacement
*/
                base_str[0] = '\0';
                dsp_size = bd_size -1;
                if (dsp_size > 0)
                  {
/*
*   check for PC relative mode
*/
		    if (ea_m != 7) dsp_size = dsp_size + 4;
                    op_value = disp_code(dsp_size,0,base_str);
                  }
/*
*   Get outer displacement if any
*/
                od_str[0] = '\0';
                dsp_size = 0;
                if (iis == 2 || iis == 6)
                  dsp_size = 5;
                else if (iis == 3 || iis == 7)
                  dsp_size = 6;
                if (dsp_size != 0) disp_code(dsp_size,0,od_str);
/*
*   If base register enabled, combine base displacement and register
*/
		strcpy(reg_str,reg_str + 1);
		reg_str[2] = '\0';
		if (bs == 0)
		  if (bd_size == 1)
		    strcpy(tmp_str,reg_str);
                  else
                    {
                      strcpy(tmp_str,base_str);
                      strcat(tmp_str,",");
		      strcat(tmp_str,reg_str);
		    }
/*
*   Base register and index register enabled
*/
                if (bs == 0 && is == 0)
                  {
                    if (iis == 0)
/*
*   (An,Xn) and (bd,An,Xn) or (PC,Xn) and (bd,PC,Xn)
*/
                      {
                        strcpy(str,"(");
                        strcat(str,tmp_str);
                        strcat(str,",");
                        strcat(str,xreg_str);
                        strcat(str,")");
                      }
                    else if (iis == 1)
/*
*   ([An,Xn]) and ([bd,An,Xn]) or ([PC,Xn]) and ([bd,PC,Xn])
*/
                      {
                        strcpy(str,"([");
                        strcat(str,tmp_str);
                        strcat(str,",");
                        strcat(str,xreg_str);
                        strcat(str,"])");
                      }
                    else if (iis <= 3)
/*
*   ([An,Xn],od) and ([bd,An,Xn],od) or ([PC,Xn],od) and ([bd,PC,Xn],od)
*/
                      {
                        strcpy(str,"([");
                        strcat(str,tmp_str);
                        strcat(str,",");
                        strcat(str,xreg_str);
                        strcat(str,"],");
                        strcat(str,od_str);
                        strcat(str,")");
                      }
		    else if (iis == 4)
			instr_valid = FALSE;
                    else if (iis == 5)
/*
*   ([An],Xn) and ([bd,An],Xn) or ([PC],Xn) and ([bd,PC],Xn)
*/
                      {
                        strcpy(str,"([");
                        strcat(str,tmp_str);
                        strcat(str,"],");
                        strcat(str,xreg_str);
                        strcat(str,")");
                      }
                    else if (iis <= 7)
/*
*   ([An],Xn,od) and ([bd,An],Xn,od) or ([PC],Xn,od) and ([bd,PC],Xn,od)
*/
                     {
                        strcpy(str,"([");
                        strcat(str,tmp_str);
                        strcat(str,"],");
                        strcat(str,xreg_str);
                        strcat(str,",");
                        strcat(str,od_str);
                        strcat(str,")");
                      }
                  }
/*
*    Base register enabled and index register disabled
*/
                else if (bs == 0 && is == 1)
                  {
                    if (iis == 0)
/*
*   (bd,An) or (db,PC)
*/
                      {
                        strcpy(str,"(");
                        strcat(str,tmp_str);
                        strcat(str,")");
                        if (ea_m == 7)
                          {
                             op_addr = TRUE;
                             op_value = op_value + pc + 2;
                          }
                      }
                    else if (iis == 1)
/*
*   ([An]) and ([bd,An]) or ([PC]) and ([bd,PC])
*/
                      {
                        strcpy(str,"([");
                        strcat(str,tmp_str);
                        strcat(str,"])");
                      }
                    else if (iis <= 3)
/*
*   ([An],od) and ([bd,An],od) or ([PC],od) and ([bd,PC],od)
*/
                      {
                        strcpy(str,"([");
                        strcat(str,tmp_str);
                        strcat(str,"],");
                        strcat(str,od_str);
                        strcat(str,")");
                      }
                    else
                      instr_valid = FALSE;
                  }
		else if (bs == 1 && is == 0)
                  {
                    if (bd_size == 1)
		      strcpy(tmp_str,xreg_str);
                    else
                      {
			strcpy(tmp_str,base_str);
			strcat(tmp_str,",");
			strcat(tmp_str,xreg_str);
                      }
                    if (iis == 0)
/*
*   (Xn) and (bd,Xn)
*/
                      {
                        strcpy(str,"(");
                        strcat(str,tmp_str);
                        strcat(str,")");
                      }
                    else if (iis == 1)
/*
*   ([Xn]) and ([bd,Xn])
*/
                      {
                        strcpy(str,"([");
                        strcat(str,tmp_str);
                        strcat(str,")]");
                      }
                    else if (iis <= 3)
/*
*   ([Xn],od) and ([bd,Xn],od)
*/
                      {
                        strcpy(str,"([");
                        strcat(str,tmp_str);
                        strcat(str,"],");
                        strcat(str,od_str);
                        strcat(str,")");
                      }
                    else
                      instr_valid = FALSE;
                  }
/*
*   Base register and index disabled
*/
                else if (bs == 1 && is == 1)
                  {
                    if (ea_r != 0) instr_valid = FALSE;
                    if (bd_size == 1) strcpy(base_str,"0");
                    if (iis == 0 || iis >= 3) instr_valid = FALSE;
/*
*   ([bd]) or ([bd],od)
*/
                    strcpy(str,"([");
		    strcat(str,base_str);
		    strcat(str,"]");
                    if (iis != 1)
                      {
                        strcat(str,",");
                        strcat(str,od_str);
                      }
		    strcat(str,")");
                  }
           }
      }
   else if (ea_m == 7 && ea_r <= 1)
/*
*   Absolute address
*/
      {
         abs_size = 9 + ea_r;
         op_value = disp_code(abs_size,0,str);
         op_addr = TRUE;
         if (ea_r == 1) 
           if (op_value > 0xffff8000 || op_value < 0x8000) warn = TRUE;
      }
   else if (ea_m == 7 && ea_r == 4)
/*
*   Immediate
*/
      imm_code(str);
   else if (ea_m == 7 && ea_r == 2)
/*
*   PC relative with displacement
*/
      {
         op_value = disp_code(1,0,str);
         op_value = op_value + pc + 2;
         op_addr = FALSE;
         strcat(str,"(PC)");
      }
   else
      instr_valid = FALSE;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$   rel_code   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*
* Routine handles conversion and formatting of relative address tables.
*/


void rel_code (int type, unsigned long base, char *str)
{
   unsigned long int temp,tmp;
   char     tmp_str[16];

/*
*   16-bit displacements
*/
   if (type == 1)
       {
         temp = get_dat(2);
         if (temp >= 0x8000) temp = temp | 0xffff0000;
       }
/*
*   32-bit displacements
*/
   else if (type == 2) temp = get_dat(4);

   num_code(temp+base,tmp_str);
   strcpy(str,"L");
   tmp = (temp + base) & 0xffff0000;
   if (tmp == 0 || tmp == 0xffff0000)
     {
       strcat(str,tmp_str+4);
     }
   else
     {
       strcat(str,tmp_str);
     }
   strcat(str,"-L");
   num_code(base,tmp_str);
   tmp = base & 0xffff0000;
   if (tmp == 0 || tmp == 0xffff0000)
     {
       strcat(str,tmp_str+4);
     }
   else
     {
       strcat(str,tmp_str);
     }
   if (ref_end == ref_end + REF_MAX)
     {
       if (ref_ovfl == 0) fprintf (stderr," Reference table overflow\n");
       ref_ovfl = 1;
     }
    else
     {
       ++ref_end;
       ref_list[ref_end] = temp + base;
     }
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$   disp_code   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*
* Routine handles conversion and formatting of address register displacements,
* PC relative displacements, and absolute addresses.  If an address is
* formated as a label, the label is inserted in the label reference list.
*/


long int disp_code (int type, char byte, char *str)
{
   long int temp,disp;
   char     tmp_str[16];

/*
*    8-bit displacement - value passed in byte
*/
   if (type == 0 || type == 4)
       temp = byte;
/*
*   16-bit displacements
*/
   else if (type == 1 || type == 5 || type == 9)
       {
         temp = (short int) get_dat(2);
       }
/*
*   32-bit displacements
*/
   else if (type == 2 || type == 6 || type == 10)
     temp = get_dat(4);
   disp = temp;

   if (type >= 4 && type <= 6)
/*
*   An displacement types. Format as HEX number.
*/
     {
       if (type == 4)
       if (temp == 0xffffff80) temp = 0x80;
       else if (type == 5)
       if (temp == 0xffff8000) temp = 0x8000;
       strcpy(str,"$");
       if (temp < 0 && temp != 0x80000000)
         {
	   strcpy(str,"-$");
           temp = -temp;
         }
       num_code(temp,tmp_str);
       if (temp < 32767)
         if (type == 4)
	   strcat(str,tmp_str + 6);
         else
	   strcat(str,tmp_str + 4);
       else
         strcat(str,tmp_str);
     }
   else
     {
/*
*   Check for PC relative types.  If not PC relative, it is absolute
*   address.  Format as a label and put in the label reference table.
*/
       if (type <= 2) temp = pc + temp + 2;
/*
*   MC68000 and MC68010 have only a 24-bit address.
*/
       if (cpu < 2) 
         {
           if (type == 9) temp = temp & 0xffffff;
           if ((temp & 0xff000000) != 0) instr_valid = FALSE;
         }
       num_code(temp,tmp_str);
       strcpy(str,"L");
       if ((temp & 0xffff0000) == 0)
	 strcat(str,tmp_str + 4);
       else
	 strcat(str,tmp_str);
       if (ref_end == ref_end + REF_MAX)
         {
           if (ref_ovfl == 0) fprintf (stderr," Reference table overflow\n");
           ref_ovfl = 1;
         }
       else
         {
           ++ref_end;
	   ref_list[ref_end] = temp;
         }
     }
   return(disp);
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$    imm_code   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*    Routine to process immediate data fields -  #<data>    */


long int imm_code (char *str)
{
   long int temp;
   unsigned long temp2;
   char     tmp_str[16];

/*
*     MC680x0 immediate sizes
*/
   if (op_size == 0)
     {
       temp = get_dat(2);
       if ((temp & 0xff00) != 0 && (temp & 0xff00) != 0xff00) 
                                                   instr_valid = FALSE;
       temp = (char) temp;
       if (temp == 0xffffff80) temp = 0x80;
       if (temp >= 0)
         strcpy(str,"#$");
       else
         strcpy(str,"#-$");
       if (temp < 0) temp = -temp;
       num_code(temp,tmp_str);
       strcat(str,tmp_str + 6);
     }
   else if (op_size == 1)
     {
       temp = (short int) get_dat(2);
       if (temp == 0xffff8000) temp = 0x8000;
       if (temp >= 0)
         strcpy(str,"#$");
       else
         strcpy(str,"#-$");
       if (temp < 0) temp = -temp;
       num_code(temp,tmp_str);
       strcat(str,tmp_str + 4);
     }
   else if (op_size >= 2 && op_size <= 12)
     {
       temp = get_dat(4);
       op_value = temp;
       op_imm = TRUE;
       temp2 = temp;
       if (temp2 >= org && temp2 <= pcend)
         {
           if (ref_end == ref_end + REF_MAX)
             {
               if (ref_ovfl == 0) printf(" Reference table overflow\n");
               ref_ovfl = 1;
             }
           else
             {
               ++ref_end;
               ref_list[ref_end] = temp;
             }
         }
       if (op_size == 2 && temp < 0 && temp > -32768)
         {
           strcpy(str,"#-$");
           temp = -temp;
         }
       else
         strcpy(str,"#$");
       num_code(temp,tmp_str);
       strcat(str,tmp_str);
       if (op_size == 2) return(temp);
/*
*   MC68881/MC68882 immediate sizes
*/
       temp = get_dat(4);
       num_code(temp,tmp_str);
       strcat(str,tmp_str);
       if (op_size == 8) return(temp);
       temp = get_dat(4);
       num_code(temp,tmp_str);
       strcat(str,tmp_str);
       if (op_size == 12) return(temp);
       instr_valid = FALSE;
     }
   else
     instr_valid = FALSE;
   return(temp);
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$    num_code   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Convert a long integer to an ASCII hexadecimal string
*/

void num_code (long int temp,char *str)
{
   static char inital[] = {"00000000"};
   char *ptr,nibble;

   strcpy(str,inital);
   ptr = str + 7;
   while (temp != 0)
     {
       nibble = (char) ((temp & 0xF) + 48);
       if (nibble > '9') nibble = nibble + 7;
       temp = (temp >> 4) & 0xfffffff;
       *ptr = nibble;
       --ptr;
     }
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$     get_dat   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Get an 8, 16 or 32-bit word from the source array.  Increments the byte
*   length (ir_len) as data are removed from the array.  At the end of the
*   instruction, ir_len reflects the length of the instruction.
*/

long int get_dat(int bytes)
{
   long int temp;
   unsigned char *ptr;

   ptr = hex_ptr + ir_len;
   ir_len = ir_len + bytes;
   temp = 0;
   while (bytes > 0)
     {
       temp = temp << 8;
       temp = temp + *ptr;
       --bytes;
       ++ptr;
     }
   return (temp);
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$    addr_modes   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Check for valid addressing mode.  Set instr_valid to FALSE is the mode
*   is not allowed.
*/

void addr_modes (enum mode mode)
{
   switch  (mode)
    {
   case  DATA:
       if (ea_m == 1) instr_valid = FALSE;
       break;
   case  MEM:
       if (ea_m <= 1) instr_valid = FALSE;
       break;
   case  CNTL:
       if (ea_m <= 1) instr_valid = FALSE;
       if (ea_m == 3 || ea_m == 4) instr_valid = FALSE;
       if (ea_m == 7 && ea_r == 4) instr_valid = FALSE;
       break;
   case  ALTER:
       if (ea_m == 7 && ea_r > 1) instr_valid = FALSE;
       break;
   case  DATA_ALTER:
       if (ea_m == 1) instr_valid = FALSE;
       if (ea_m == 7 && ea_r > 1) instr_valid = FALSE;
       break;
   case  MEM_ALTER:
       if (ea_m <= 1) instr_valid = FALSE;
       if (ea_m == 7 && ea_r > 1) instr_valid = FALSE;
       break;
   case  CNTL_ALTER:
       if (ea_m <= 1) instr_valid = FALSE;
       if (ea_m == 3 || ea_m == 4) instr_valid = FALSE;
       if (ea_m == 7 && ea_r > 1) instr_valid = FALSE;
       break;
   default:
       fprintf (stderr,"\nUrecognized addressing mode = %i\n",mode);
       break;
    }
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     rxry_code   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Encode operand field for CMPM, ABCD, SBCD, PACK, UNPK and SUBX
*   instructions.
*/

void rxry_code (int type,char *sorc_str,char *dest_str)
{
   int   rx,ry;

   ry = (int) (instr_wd & 0xF);
   rx = (int) ((instr_wd >> 9) & 7);
   if (ry > 7) rx = rx + 8;
   if (ry <= 7)
     {
       strcpy(sorc_str,*(reg_codes + ry));
       strcpy(dest_str,*(reg_codes + rx));
     }
   else if (type == 0)
/*
*   Address register with postincrement
*/
     {
       strcpy(sorc_str,"(");
       strcat(sorc_str,*(reg_codes + ry));
       strcat(sorc_str,")+");
       strcpy(dest_str,"(");
       strcat(dest_str,*(reg_codes + rx));
       strcat(dest_str,")+");
     }
   else
     {
/*
*   Address register with predecrement
*/
       strcpy(sorc_str,"-(");
       strcat(sorc_str,*(reg_codes + ry));
       strcat(sorc_str,")");
       strcpy(dest_str,"-(");
       strcat(dest_str,*(reg_codes + rx));
       strcat(dest_str,")");
     }
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     reg_list    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*    Encode a register list for MC680x0 MOVEM instructions.
*/

void reg_list (char *str)
{
   int   creg = 0;   /*  current register      */
   int   lastr = 0;  /*  last register in list */
   int   lastb = 0;  /*  last bit test value   */
   unsigned int   msk = 1;    /*  bit test mask         */
   int   sfd = 1;    /*  shift direction. positive = left, negative = right*/
   long int temp;
   char  *ptr;

   ptr = str;
   *ptr = '\0';
   if (ea_m == 4) {sfd = -sfd;  msk = 0x8000;}
   temp = get_dat(2);

   while (creg < 16)
     {
       if ((temp & msk) == 0)
         {
           if (lastb == 1)
             if (creg - lastr > 1)
	       {
		 *ptr++ = '-';
		 strcpy(ptr,*(reg_codes + creg-1));
		 ptr += 2;
               }
           lastb = 0;
         }
       else
         {
           if (lastb == 0)
             {
               if (ptr != str) {*ptr = '/'; ++ptr;}
	       strcpy(ptr,*(reg_codes + creg));
               ptr = ptr + 2;
               lastr = creg;
             }
           lastb = 1;
         }
       if (sfd > 0)
          msk <<= 1;
       else
          msk >>= 1;
       creg += 1;
     }
   *ptr = '\0';
   if (lastb == 1 && lastr != 15) strcat(ptr,"-A7");
   ptr = strchr(str,(int) 'S');
   if (ptr != 0) strncpy(ptr,"A7",2);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     dcb_code    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Encode DC.B blocks.  If the byte is a printing ACSII character, the
*   output is formated as ACSII.  Otherwise, it is formated as ASCII Hex.
*
*/

void dcb_code (unsigned long int n,char *str)
{
   int    ni,asc = 0;
   char  *ptr,xchar,tmp_str[12];

   ptr = str;
   if (n > 6)
     ni = 6;
   else
     ni = (int) n;
   while (ni > 0)
     {
       xchar = (char) get_dat(1);
       if (xchar < ' ' || xchar == 0x7b || xchar == 0x7d || xchar >= 0x7f)
         {
           num_code((long) xchar,tmp_str);
           if (asc == 1) *ptr++ = '\'';
	   if (ptr != str) *ptr++ = ',';
	   *ptr++ ='$';
	   strncpy(ptr,tmp_str + 6,2);
           ptr += 2;
           asc = 0;
         }
       else
         {
           if (asc == 0) 
             {
               if (ptr == str)
                 *ptr++ = '\'';
               else
                 {
                   *ptr++ = ',';
                   *ptr++ = '\'';
                 }
             }
	   *ptr++ = xchar;
           if (xchar == '\'') *ptr++ = xchar;
           asc = 1;
         }
       ni -= 1;
     }
   if (asc == 1) *ptr++ = '\'';
   *ptr = '\0';
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     dcw_code    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Encode a DC.W block.  Output is 16-bit ASCII hex.
*/


void dcw_code (unsigned long int n,char *str)
{
   int    ni;
   unsigned long int temp;
   char  *ptr,tmp_str[12];

   ptr = str;
   *ptr ='\0';
   if (n > 8)
     ni = 4;
   else
     ni = (int) (n/2);
   if (ni == 0) ni = 1;
   while (ni > 0)
     {
       temp = get_dat(2);
       num_code(temp,tmp_str);
       if (ptr == str)
         *ptr++ = '$';
       else
         {
           *ptr++ = ',';
           *ptr++ = '$';
         }
       strcpy(ptr,tmp_str + 4);
       ptr += 4;
       ni -= 1;
     }
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     dcl_code    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Encode a DC.L block.  Output is 32-bit ACSII hex.
*/

void dcl_code (unsigned long int n,char *str)
{
   int    ni;
   long int  temp;
   char  *ptr,tmp_str[12];

   ptr = str;
   *ptr = '\0';
   if (n > 8)
     ni = 2;
   else
     ni = (int) (n/4);
   if (ni == 0) ni = 1;
   while (ni > 0)
     {
       temp = get_dat(4);
       num_code(temp,tmp_str);
       if (ptr == str)
         *ptr++ = '$';
       else
         {
           *ptr++ = ',';
           *ptr++ = '$';
         }
       strcpy(ptr,tmp_str);
       ptr += 8;
       ni -= 1;
     }
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$      order      $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Sort the label list into numerical order (unsigned 32-bit)
*/

void  order (void)
{
   int    i,j,l,m;
   unsigned long int temp;

   if (ref_end <= 1) return;
   m = ref_end + 1;
   while (m/2 != 0)
     {
       m = m/2;
       j = 0;
       while (j <= ref_end - m)
	 {
	   i = j;
	   do
	     {
	       l = i + m;
	       if (ref_list[i] > ref_list[l])
		 {
		   temp = ref_list[i];
		   ref_list[i] = ref_list[l];
		   ref_list[l] = temp;
		 }
	       i -= m;
	     }
	   while (i >= 0);
	   j += 1;
	 }
     }
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     ref_bld     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Routine maintains a list of addresses referenced.  On pass 1 of the
*   disassembly, the list is built.  After pass 1, the list is sorted
*   into numerical order.  On pass 2, the list is used to determine when
*   a label should be output.
*/

void  ref_bld (void)
{
   int    i;
   unsigned long int temp;

   while (ref_end != ref_last)
     {
       i = ref_last;
       while (i >= 0)
         {
	   if (ref_list[ref_end] == ref_list[i])
             {
               ref_end -= 1;
               goto iloop;
             }
           i -= 1;
         }
       ref_last += 1;
       temp = ref_list[ref_last];
       ref_list[ref_last] = ref_list[ref_end];
       ref_list[ref_end] = temp;
iloop:;
     }
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_0    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
    CALLM, CAS, CAS2, CHK2, CMP2, MOVEP, MOVES, RTM, Bit manipulation,
    and Immdeiate data instructions.
*/

void class_0 (void)
{
   char  sorc_str[38],dest_str[38],tmp_str[38];
   static char *bit_codes[] = {"BTST","BCHG","BCLR","BSET"};
   static char *ctl_codes[] = {"CCR","SR",""};
   long int temp,temp2;

   if (dest_m >= 4)
/*
*    MOVEP and bit manipulation (Dynamic) instructions
*/
     {
       strcpy(sorc_str,*(reg_codes + dest_r));
       if (ea_m == 1)
         {
	   strcpy(instr_str,"MOVEP");
           op_size = 1;
           if ((dest_m & 1) == 1) 
            {
              strcpy(instr_str,"MOVEP.L");
              op_size = 2;
            }
           ea_m = 5;
           ea_code(tmp_str);
           if (dest_m >= 6)
             strcpy(dest_str,tmp_str);
           else
             {
               strcpy(dest_str,sorc_str);
               strcpy(sorc_str,tmp_str);
             }
         }
       else
/*
*   BCHG, BCLR, BSET and BTST of form  BXXX  Dn,<ea>
*/
         {
	   strcpy(instr_str,*(bit_codes + op_size));
           if (op_size == 0)
/*
*   BTST
*/
             addr_modes(DATA);
           else
	     addr_modes(DATA_ALTER);
           ea_code(dest_str);
           op_size = 0;
         }
     }
   else if (dest_r == 4)
/*
*    Bit manipulation (static).    BXXX  #<data>,<ea>
*/
     {
       strcpy(instr_str,*(bit_codes + op_size));
       if (op_size != 0) addr_modes(DATA_ALTER);
       op_size = 0;
       imm_code(sorc_str);
       ea_code(dest_str);
     }
   else if (op_size <= 2)
/*
*    MOVES and data immediate instructions.  Some immediate instructions
*    us #<data> effective address mode to reference SR and CCR registers.
*/
     {
       if (dest_r == 0)
         {
           strcpy(instr_str,"ORI");
           imm_code(sorc_str);
           if (ea_m == 7 && ea_r == 4)
/*
*    ORI  #<data>,SR  and  ORI  #<data>,CCR
*/
             {
	       strcpy(dest_str,*(ctl_codes + op_size));
               if (op_size == 2) instr_valid = FALSE;
             }
           else
/*
*    ORI  #<data>,<ea>
*/
             {
	       strcat(instr_str,*(size_codes + op_size));
               addr_modes(DATA_ALTER);
               ea_code(dest_str);
             }
         }
       else if (dest_r == 1)
         {
           strcpy(instr_str,"ANDI");
           imm_code(sorc_str);
           if (ea_m == 7 && ea_r == 4)
/*
*    ANDI  #<data>,SR  and  ANDI  #<data>,CCR
*/
             {
	       strcpy(dest_str,*(ctl_codes + op_size));
               if (op_size == 2) instr_valid = FALSE;
             }
           else
/*
*    ANDI  #<data>,<ea>
*/
             {
	       strcat(instr_str,*(size_codes + op_size));
               addr_modes(DATA_ALTER);
               ea_code(dest_str);
             }
         }
       else if (dest_r == 2)
/*
*    SUBI  #<data>,<ea>
*/
         {
           strcpy(instr_str,"SUBI");
	   strcat(instr_str,*(size_codes + op_size));
           imm_code(sorc_str);
	   addr_modes(DATA_ALTER);
           ea_code(dest_str);
         }
       else if (dest_r == 3)
/*
*    ADDI  #<data>,<ea>
*/
         {
           strcpy(instr_str,"ADDI");
	   strcat(instr_str,*(size_codes + op_size));
           imm_code(sorc_str);
	   addr_modes(DATA_ALTER);
           ea_code(dest_str);
         }
       else if (dest_r == 5)
         {
           strcpy(instr_str,"EORI");
           imm_code(sorc_str);
           if (ea_m == 7 && ea_r == 4)
/*
*    EORI  #<data>,SR  and  EORI  #<data>,CCR
*/
             {
	       strcpy(dest_str,*(ctl_codes + op_size));
               if (op_size == 2) instr_valid = FALSE;
             }
           else
/*
*    EORI  #<data>,<ea>
*/
             {
	       strcat(instr_str,*(size_codes + op_size));
               addr_modes(DATA_ALTER);
               ea_code(dest_str);
             }
         }
       else if (dest_r == 6)
/*
*    CMPI  #<data>,<ea>
*/
         {
	   strcpy(instr_str,"CMPI");
	   strcat(instr_str,*(size_codes + op_size));
           imm_code(sorc_str);
           addr_modes(DATA);
           ea_code(dest_str);
         }
       else
         {
/*
*    MOVES  Rn,<ea>  and  MOVES  <ea>,Rn
*
*  MC68010 up
*/
           if (cpu == 0) instr_valid = FALSE;
           addr_modes(MEM_ALTER);
           strcpy(instr_str,"MOVES");
	   strcat(instr_str,*(size_codes + op_size));
           temp = get_dat(2);
           if ((temp & 0x7FF) != 0) instr_valid = FALSE;
	   strcpy(sorc_str,*(reg_codes + (temp >> 12)));
           if ((temp & 0x800) != 0)
             ea_code(dest_str);
           else
             {
               strcpy(dest_str,sorc_str);
	       ea_code(sorc_str);
             }
         }
     }
   else if (op_size == 3)
     {
       if (dest_r <= 2)
/*
*    CHK2  <ea>,Rn  and   CMP2  <ea>,Rn
*
*  MC68020 up
*/
         {
           if (cpu < 2) instr_valid = FALSE;
           temp = get_dat(2);
           if ((temp & 0x7FF) != 0) instr_valid = FALSE;
           op_size = dest_r;
           addr_modes(CNTL);
           ea_code(sorc_str);
           if ((temp & 0x800) != 0)
             strcpy(instr_str,"CHK2");
           else
             strcpy(instr_str,"CMP2");
	   strcat(instr_str,*(size_codes + op_size));
	   strcpy(dest_str,*(reg_codes + (temp >> 12)));
         }
       else if (dest_r == 3)
/*
*    RTM  Rn
*
*  MC68020 only
*/
         {
           if (cpu != 2) instr_valid = FALSE;
           if (ea_m <= 1)
             {
               strcpy(instr_str,"RTM");
               *dest_str = '\0';
               if (ea_m == 1) ea_r += 8;
	       strcpy(sorc_str,*(reg_codes + ea_r));
             }
           else
/*
*    CALLM  #<data>,<ea>
*
*  MC68020 only
*/
             {
               strcpy(instr_str,"CALLM");
               op_size = 0;
               imm_code(sorc_str);
               addr_modes(CNTL);
               ea_code(dest_str);
             }
         }
       else if (dest_r >= 4)
/*
*    CAS  and  CAS2
*
*  MC68020 up
*/
         {
           if (cpu < 2) instr_valid = FALSE;
           op_size = (dest_r - 1) & 3;
           if (ea_m == 7 && ea_r == 4)
/*
*    CAS2  Dc1:Dc2,Du1:Du2,(Rn1):(Rn2)
*/
             {
               if (op_size == 0) instr_valid = FALSE;
               temp = get_dat(2);
               if ((temp & 0x0E38) != 0) instr_valid = FALSE;
               temp2 = get_dat(2);
               if ((temp2 & 0x0E38) != 0) instr_valid = FALSE;
	       strcpy(sorc_str,*(reg_codes + (temp & 7)));
               strcat(sorc_str,":");
	       strcat(sorc_str,*(reg_codes + (temp2 & 7)));
               strcat(sorc_str,",");
	       strcat(sorc_str,*(reg_codes + ((temp >> 6) & 7)));
               strcat(sorc_str,":");
	       strcat(sorc_str,*(reg_codes + ((temp2 >> 6) & 7)));
               strcat(sorc_str,",(");
	       strcat(sorc_str,*(reg_codes + (temp >> 12)));
               strcat(sorc_str,"):(");
	       strcat(sorc_str,*(reg_codes + (temp2 >> 12)));
               strcat(sorc_str,")");
               strcpy(instr_str,"CAS2");
	       strcat(instr_str,*(size_codes + op_size));
               *dest_str = '\0';
             }
           else
/*
*    CAS  Dc,Du,<ea>
*/
             {
               strcpy(instr_str,"CAS");
               strcat(instr_str,size_codes[op_size]);
               temp = get_dat(2);
               if ((temp & 0xFE38) != 0) instr_valid = FALSE;
	       strcpy(sorc_str,*(reg_codes + (temp & 7)));
               strcat(sorc_str,",");
	       strcat(sorc_str,*(reg_codes + ((temp >> 6) & 7)));
	       addr_modes(MEM_ALTER);
               ea_code(dest_str);
             }
         }      
     }
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_1     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*   Process  MOVE.B instructions.
*/

void class_1 (void)
{
   char  sorc_str[38],dest_str[38];

   op_size = 0;
   strcpy(instr_str,"MOVE.B");
   ea_code(sorc_str);
   ea_m = dest_m;
   ea_r = dest_r;
   addr_modes(DATA_ALTER);
   ea_code(dest_str);
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_2     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*  Process MOVE.L instructions.
*/

void class_2 (void)
{
   char  sorc_str[38],dest_str[38];

   op_size = 2;
   if (dest_m == 1)
     strcpy(instr_str,"MOVEA.L");
   else
     strcpy(instr_str,"MOVE.L");
   ea_code(sorc_str);
/*
*   Only the MC68020 up allow MOVE.L at starting at an odd address
*/
   if (cpu < 2)
     if (op_addr == TRUE && (op_value & 1) != 0) instr_valid = FALSE;
/*
*   Assembliers may convert MOVE.L #<data>,Dn to MOVEQ  #<data>,Dn
*   if the immdeiate data is small.
*/
   if (op_imm == TRUE && dest_m == 0)
     if (op_value > -128 && op_value < 127) warn = TRUE;
   ea_m = dest_m;
   ea_r = dest_r;
   if (dest_m != 1) addr_modes(DATA_ALTER);
   ea_code(dest_str);
   instr_finis(sorc_str,dest_str);
   return;                  
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_3     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Process MOVE.W instructions.
*/

void class_3 (void)
{
   char  sorc_str[38],dest_str[38];

   op_size = 1;
   if (dest_m == 1)
     strcpy(instr_str,"MOVEA.W");
   else
     strcpy(instr_str,"MOVE.W");
   ea_code(sorc_str);
/*
*   Only the MC68020 up allow MOVE.L at starting at an odd address
*/
   if (cpu < 2)
     if (op_addr == TRUE && (op_value & 1) != 0) instr_valid = FALSE;
   ea_m = dest_m;
   ea_r = dest_r;
   if (dest_m != 1) addr_modes(DATA_ALTER);
   ea_code(dest_str);
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_4     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Process miscellaneous instructions.
*/

void class_4 (void)
{
   char  sorc_str[38],dest_str[38],tmp_str[38];
   static char *creg_codes[] = {"SFC","DFC","CACR","TC",
                                "ITT0","ITT1","DTT0","DTT1",
                                "USP","VBR","CAAR","MSP",
                                "ISP","MMUSR","URP","SRP"};
   int      tf = 0;
   long int temp;

   *dest_str = '\0';
   if (dest_m <= 3)
     {
       if (dest_r <= 1 && dest_m == 3)
/*
*    MOVE  SR,<ea>  and  MOVE  CCR,<ea>
*/
	 {
	   addr_modes(DATA_ALTER);
	   strcpy(instr_str,"MOVE");
	   if (dest_r == 0)
	     {
	       strcpy(sorc_str,"SR");
               op_size = 1;
	     }
	   else
/*
*    MOVE  CCR,<ea>
*
*  MC68010 up
*/
	     {
	       if (cpu < 1) instr_valid = FALSE;
	       strcpy(sorc_str,"CCR");
               op_size = 0;
	     }
	   ea_code(dest_str);
	 }
       else if (dest_r <= 3 && dest_m == 3)
/*
*    MOVE  <ea>,SR  and  MOVE  <ea>,CCR
*/
	 {
	   addr_modes(DATA);
	   strcpy(instr_str,"MOVE");
	   if (dest_r == 2)
	     {
	       strcpy(dest_str,"CCR");
	       op_size = 0;
	     }
	   else
	     {
	       strcpy(dest_str,"SR");
	       op_size = 1;
	     }
	   ea_code(sorc_str);
	 }
       else if (dest_r <= 3)
/*
*    CLR, NEG, NEGX and NOT - <ea>
*/
	 {
	   if (dest_r == 0)
	     strcpy(instr_str,"NEGX");
	   else if (dest_r == 1)
	     strcpy(instr_str,"CLR");
	   else if (dest_r == 2)
	     strcpy(instr_str,"NEG");
	   else
	     strcpy(instr_str,"NOT");
	   strcat(instr_str,*(size_codes + op_size));
	   addr_modes(DATA_ALTER);
	   ea_code(sorc_str);
	 }
       else if (dest_r == 4)
	 {
	   if (dest_m == 0)
/*
*    LINK.L  An,#<displacement>
*
*  MC68020 up
*/
	     {
	       if (cpu < 2) instr_valid = FALSE;
	       if (ea_m == 1)
		 {
		   strcpy(instr_str,"LINK.L");
		   strcpy(sorc_str,*(reg_codes + ea_r + 8));
		   op_size = 2;
		   imm_code(dest_str);
		 }
	       else
/*
*    NBCD  <ea>
*/
		 {
		   strcpy(instr_str,"NBCD");
		   addr_modes(DATA_ALTER);
		   ea_code(sorc_str);
		 }
	     }
	   else if (dest_m == 1)
	     {
	       if (ea_m == 0)
/*
*    SWAP  Dn
*/
		 {
		   strcpy(instr_str,"SWAP");
		   strcpy(sorc_str,*(reg_codes + ea_r));
		 }
	       else if (ea_m == 1)
/*
*    BKPT  #<data>
*
*  MC68010 thur MC68030
*/
		 {
		   if (cpu == 0 || cpu > 3) instr_valid = FALSE;
		   strcpy(instr_str,"BKPT");
		   strcpy(sorc_str,"# ");
		   sorc_str[1] = (char) (ea_r + 0x30);
		 }
	       else
		 {
/*
*    PEA  <ea>
*/
		   strcpy(instr_str,"PEA");
		   addr_modes(CNTL);
		   ea_code(sorc_str);
		   op_addr = FALSE;
		 }
	     }
	   else if (dest_m == 2 || dest_m == 3)
	     {
	       if (ea_m == 0)
/*
*    EXT  Dn  or  EXT.L Dn
*/
		 {
		   strcpy(instr_str,"EXT");
		   strcpy(sorc_str,*(reg_codes + ea_r));
		 }
	       else
		 {
/*
*    MOVEM(.L)  <register list>,<ea>
*/
		   strcpy(instr_str,"MOVEM");
		   if (ea_m != 4) addr_modes(CNTL_ALTER);
		   reg_list(sorc_str);
		   ea_code(dest_str);
		 }
               op_size = 1;
	       if (dest_m == 3)
                {
                 strcat(instr_str,".L");
                 op_size = 2;
                }
	     }
	 }
       else if (dest_r == 5)
	 {
	   if (dest_m == 3)
	     {
	       if (ea_m == 7 && ea_r == 4)
		 {
/*
*    ILLEGAL
*/
		   strcpy(instr_str,"ILLEGAL");
		   *sorc_str = '\0';
		 }
	       else
		 {
/*
*    TAS  <ea>
*/
		   strcpy(instr_str,"TAS");
		   addr_modes(DATA_ALTER);
		   ea_code(sorc_str);
                   op_size = 0;
		 }
	     }
	   else
	     {
/*
*    TST  <ea>
*/
	       strcpy(instr_str,"TST");
	       strcat(instr_str,*(size_codes + op_size));
	       if (ea_m == 7 && ea_r == 4) instr_valid = FALSE;
	       ea_code(sorc_str);
	     }
	 }
       else if (dest_r == 6)
	 {
	   if (dest_m == 0)
	     {
/*
*    MULU.L/MULS.L   <ea>,Dl or <ea>,Dh:Dl
*
*  MC68020 up
*/
	       if (cpu < 2) instr_valid = FALSE;
	       temp = get_dat(2);
	       strcpy(instr_str,"MULU.L");
	       if ((temp & 0x800) != 0) strcpy(instr_str,"MULS.L");
	       if ((temp & 0x83F8) != 0) instr_valid = FALSE;
	       if ((temp & 0x400) != 0)
		 {
		   strcpy(dest_str,*(reg_codes + (temp & 7)));
		   strcat(dest_str,":");
		 }
	       strcat(dest_str,*(reg_codes + (temp >> 12)));
	       op_size = 2;
	       addr_modes(DATA);
	       ea_code(sorc_str);
	     }
	   else if (dest_m == 1)
	     {
/*
*    DIVU.L/DIVS.L/DIVSL.L  -  <ea>,Dq  or  <ea>,Dr:Dq
*
*  MC68020 up
*/
	       if (cpu < 2) instr_valid = FALSE;
	       temp = get_dat(2);
	       if ((temp & 0x83F8) != 0) instr_valid = FALSE;
	       if ((temp & 0x800) != 0)
		 strcpy(instr_str,"DIVS");
	       else
		 strcpy(instr_str,"DIVU");
	       if ((temp >> 12) != (temp & 7))
		 {
		   if ((temp & 0x400) == 0) strcat(instr_str,"L");
		   strcpy(dest_str,*(reg_codes + (temp & 7)));
		   strcat(dest_str,":");
		 }
	       strcat(instr_str,".L");
	       strcat(dest_str,*(reg_codes + (temp >> 12)));
	       op_size = 2;
	       addr_modes(DATA);
	       ea_code(sorc_str);
	     }
	   else if (dest_m == 2 || dest_m == 3)
	     {
/*
*    MOVEM(.L)  <ea>,<register list>
*/
	       strcpy(instr_str,"MOVEM");
               op_size = 1;
	       if (dest_m == 3)
                {
                  strcat(instr_str,".L");
                  op_size = 2;
                }
	       if (ea_m != 3) addr_modes(CNTL);
	       reg_list(dest_str);
	       ea_code(sorc_str);
	     }
	 }
       else if (dest_r == 7)
	 {
	   if (dest_m == 0)
	     {
	     instr_valid = FALSE;
	     }
	   else if (dest_m == 1)
	     {
	       *sorc_str = '\0';
	       if (ea_m <= 1)
		 {
/*
*    TRAP  #<vector>
*/
		   strcpy(instr_str,"TRAP");
		   num_code(instr_wd & 0xF,tmp_str);
		   strcpy(sorc_str,"#$");
		   strcat(sorc_str,tmp_str + 7);
		 }
	       else if (ea_m == 2)
		 {
/*
*    LINK  An,#<displacement>
*/
		   strcpy(instr_str,"LINK");
		   strcpy(sorc_str,*(reg_codes + ea_r + 8));
		   imm_code(dest_str);
		 }
	       else if (ea_m == 3)
		 {
/*
*    UNLK  An
*/
		   strcpy(instr_str,"UNLK");
		   strcpy(sorc_str,*(reg_codes + ea_r + 8));
		 }
	       else if (ea_m == 4)
		 {
/*
*    MOVE  An,USP
*/
		   strcpy(instr_str,"MOVE");
		   strcpy(dest_str,"USP");
		   strcpy(sorc_str,*(reg_codes + ea_r + 8));
		 }
	       else if (ea_m == 5)
		 {
/*
*    MOVE  USP,An
*/
		   strcpy(instr_str,"MOVE");
		   strcpy(sorc_str,"USP");
		   strcpy(dest_str,*(reg_codes + ea_r + 8));
		 }
	       else if (ea_m == 6)
		 {
/*
*    NOP, RESET, RTD, RTE, RTR, RTS, STOP, TRAPV instructions
*/
		   if (ea_r == 0)
		     strcpy(instr_str,"RESET");
		   else if (ea_r == 1)
		     strcpy(instr_str,"NOP");
		   else if (ea_r == 2)
		     {
		       strcpy(instr_str,"STOP");
		       imm_code(sorc_str);
		     }
		   else if (ea_r == 3)
		     strcpy(instr_str,"RTE");
		   else if (ea_r == 4)
		     {
		       if (cpu == 0) instr_valid = FALSE;
		       strcpy(instr_str,"RTD");
		       imm_code(sorc_str);
		     }
		   else if (ea_r == 5)
		     strcpy(instr_str,"RTS");
		   else if (ea_r == 6)
		     strcpy(instr_str,"TRAPV");
		   else if (ea_r == 7)
		     strcpy(instr_str,"RTR");
		 }
	       else if (ea_m == 7)
		 {
/*
*    MOVE  to/from  CAAR, CACR, DFC, ISP, MSP, SFC, USP OR VBR
*
*  MC68010 implements only a subset of the registers
*/
		  if (dest_m == 1)
		    {
		      if (ea_r < 2 || ea_r > 3) instr_valid = FALSE;
		      strcpy(instr_str,"MOVEC");
		      temp = get_dat(2);
		      strcpy(tmp_str,*(reg_codes + (temp >> 12)));
		      temp = temp & 0xFFF;
                      tf = (int) (temp & 7);
		      if (temp >= 0x800 && temp <= 0x807) tf += 8;
		      else if (temp > 7) instr_valid = FALSE;
/*
*  Invalid for MC68000
*/
		      if (cpu == 0) instr_valid = FALSE;
/*
*  Only registers DFC, SFC, USP and VBR for MC68010
*/
		      if (cpu == 1)
			{
			  if (tf != 0 && tf != 1 && tf != 8 && tf != 9)
							  instr_valid =FALSE;
			}
                      else if (cpu == 2)
                        {
                          if (tf > 2 && tf < 8 || tf > 12) instr_valid = FALSE;
                        }
                      else if (cpu == 4)
                        {
                          if (tf == 10) instr_valid = FALSE;
                        }
		      if (ea_r == 2)
			{
			  strcpy(dest_str,tmp_str);
			  strcpy(sorc_str,*(creg_codes + tf));
			}
		      else
			{
			  strcpy(sorc_str,tmp_str);
			  strcpy(dest_str,*(creg_codes + tf));
			}
		    }
		 }
	     }
	   else if (dest_m == 2 || dest_m == 3)
	     {
/*
*    JSR  <ea>  or  JMP  <ea>
*/
	       if (dest_m == 2)
		 strcpy(instr_str,"JSR");
	       else
		 strcpy(instr_str,"JMP");
	       if (ea_m == 7 && ea_r == 1)
		 strcat(instr_str,".L");
	       else
		 addr_modes(CNTL);
	       ea_code(sorc_str);
	       if (op_addr && (op_value & 1) != 0) instr_valid = FALSE;
	     }
	 }
     }
   else if (dest_m == 4 || dest_m == 6)
     {
/*
*    CHK  <ea>,Dn
*
*  For MC68000 or MC68010, size can only be WORD
*/
	op_size = 1;
	if (dest_m == 4)
	  {
	    op_size = 2;
	    if (cpu < 2) instr_valid = FALSE;
	  }
	strcpy(instr_str,"CHK");
	strcat(instr_str,*(size_codes + op_size));
	strcpy(dest_str,*(reg_codes + dest_r));
	addr_modes(DATA);
	ea_code(sorc_str);
     }
   else if (dest_m == 5)
     instr_valid = FALSE;
   else if (dest_m == 7)
     {
       if (ea_m == 0)
/*
*    EXTB.L  Dn
*
*  MC68020 up
*/
	 {
	   if (cpu < 2) instr_valid = FALSE;
	   strcpy(instr_str,"EXTB.L");
	   strcpy(sorc_str,*(reg_codes + ea_r ));
	 }
       else
	 {
/*
*    LEA  <ea>,An
*/
	   strcpy(instr_str,"LEA");
	   strcpy(dest_str,*(reg_codes + dest_r + 8));
	   addr_modes(CNTL);
	   ea_code(sorc_str);
	   op_addr = FALSE;
	 }
     }
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_5     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*     Process ADDQ, SUBQ, DBcc, Scc, and TRAPcc instructions.
*/

void class_5 (void)
{
   char  sorc_str[38],dest_str[38];
   long int disp;

   *dest_str = '\0';
   if (op_size <= 2)
/*
*    ADDQ/SUBQ  #<data>
*/
     {
       if ((instr_wd & 0x100) == 0)
         strcpy(instr_str,"ADDQ");
       else
         strcpy(instr_str,"SUBQ");
       strcat(instr_str,*(size_codes + op_size));
       if (dest_r == 0) dest_r = 8;
       strcpy(sorc_str,"# ");
       sorc_str[1] = (char) (dest_r + 0x30);
       addr_modes(ALTER);
       ea_code(dest_str);
     }
   else
     {
       if (ea_m == 1)
/*
*    DBcc  Dn,<ea>
*/
         {
           if (op_cc == 1)
             strcpy(instr_str,"DBRA");
           else
             {
               strcpy(instr_str,"DB");
	       strcat(instr_str,*(cc_codes + op_cc));
	     }
	   strcpy(sorc_str,*(reg_codes + ea_r));
           disp = disp_code(1,0,dest_str);
           if (disp > -2 && disp < 2) instr_valid = FALSE;
           if ((disp & 1) != 0) instr_valid = FALSE;
         }
       else if (ea_m == 7)
         {
           if (ea_r >= 2 && ea_r <= 4)
/*
*    TRAPcc  #<ea>
*/
             {
               if (cpu == 0) instr_valid = FALSE;
               strcpy(instr_str,"TRAP");
	       strcat(instr_str,*(cc_codes + op_cc));
	       *sorc_str = '\0';
	       op_size = ea_r - 1;
	       if (ea_r != 4) imm_code(sorc_str);
             }
           else
/*
*    Scc  <ea>
*/
             {
               strcpy(instr_str,"S");
	       strcat(instr_str,*(cc_codes + op_cc));
	       addr_modes(DATA_ALTER);
               ea_code(sorc_str);
               op_size = 0;
             }
         }
       else
         {
           strcpy(instr_str,"S");
	   strcat(instr_str,*(cc_codes + op_cc));
           addr_modes(DATA_ALTER);
           ea_code(sorc_str);
         }
     }
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_6     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*    Process Bcc, BRA and BSR instructions.
*/

void class_6 (void)
{
   char  sorc_str[38];
   static char *bsize_codes[] = {".S","",".L"};
   static char *br_codes[] = {"BRA","BSR","BHI","BLS","BCC","BCS","BNE","BEQ",
                         "BVC","BVS","BPL","BMI","BGE","BLT","BGT","BLE"};
   long int  temp,disp;

   temp = instr_wd & 0xFF;
   if (temp == 0)
     op_size = 1;
   else if (temp == 0xFF)
     {
/*
*  MC68020 up
*/
       if (cpu < 2) instr_valid = FALSE;
       op_size = 2;
     }
   else
     op_size = 0;
   strcpy(instr_str,*(br_codes + op_cc));
   disp = disp_code(op_size,(char)temp,sorc_str);
   if (disp >= 0) strcat(instr_str,*(bsize_codes + op_size));
/*
*  Check for cases which may assemble with different displacement
*  length.
*/
   if (op_size == 1)
     {
       if (disp >= -128 && disp <= -2) warn = TRUE;
       if (disp > 2 && disp < 128) warn = TRUE;
     }
   else if (op_size == 2)
     if (disp >= -32768 && disp < 32768) warn = TRUE;
   if (disp > -2 && disp < ir_len - 2) instr_valid = FALSE;
   if ((disp & 1) != 0) instr_valid = FALSE;
   instr_finis(sorc_str,"");
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_7     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Process MOVEQ instructions.
*/

void class_7 (void)
{
   char  sorc_str[38],dest_str[38];

   if ((instr_wd & 0x100) == 0)
     {
       strcpy(instr_str,"MOVEQ");
       num_code(instr_wd & 0xFF,dest_str);
       strcpy(sorc_str,"#$");
       strcat(sorc_str,dest_str + 6);
       strcpy(dest_str,*(reg_codes + dest_r));
     }
   else
     instr_valid = FALSE;
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_8     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Process OR, SBCD, DIVU.W, DIVS.W, PACK and UNPK instructions.
*/

void class_8 (void)
{
   char  sorc_str[38],dest_str[38],tmp_str[12];
   long int temp;

   if (ea_m <= 1 && dest_m >= 4 && dest_m <= 6)
/*
*    SBCD, PACK and UNPK
*/
     {
       if (dest_m == 4)
         strcpy(instr_str,"SBCD");
       else if (dest_m == 5)
/*
*    MC68020 up
*/
         {
           if (cpu < 2) instr_valid = FALSE;
           strcpy(instr_str,"PACK");
         }
       else if (dest_m == 6)
/*
*    MC68020 up
*/
         {
           if (cpu < 2) instr_valid = FALSE;
           strcpy(instr_str,"UNPK");
         }
       rxry_code(1,sorc_str,dest_str);
       if (dest_m != 4)
         {
           temp = get_dat(2);
           num_code(temp,tmp_str);
           strcat(dest_str,",#$");
	   strcat(dest_str,tmp_str + 4);
         }
     }
   else if (dest_m == 3 || dest_m == 7)
/*
*    DIVU.W/DIVS.W  <ea>,Dn
*/
     {
       strcpy(instr_str,"DIVU.W");
       if (dest_m == 7) strcpy(instr_str,"DIVS.W");
       strcpy(dest_str,*(reg_codes + dest_r));
       op_size = 1;
       addr_modes(DATA);
       ea_code(sorc_str);
     }
   else
/*
*    OR  <ea>,Dn  and   OR  Dn,<ea>
*/
     {
       strcpy(instr_str,"OR");
       strcat(instr_str,*(size_codes + op_size));
       strcpy(dest_str,*(reg_codes + dest_r));
       if (dest_m < 4)
         {
           addr_modes(DATA);
           ea_code(sorc_str);
         }
       else
         {
           strcpy(sorc_str,dest_str);
           addr_modes(MEM_ALTER);
           ea_code(dest_str);
         }
     }
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_9     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Process SUB, SUBA and SUBX instructions.
*/

void class_9 (void)
{
   char  sorc_str[38],dest_str[38];

   if (dest_m == 3)
/*
*    SUBA  <ea>,An
*/
     {
       strcpy(instr_str,"SUBA");
       op_size = 1;
       strcpy(dest_str,*(reg_codes + dest_r + 8));
       ea_code(sorc_str);
     }
   else if (dest_m == 7)
/*
*    SUBA.L  <ea>,An
*/
     {
       strcpy(instr_str,"SUBA.L");
       op_size = 2;
       strcpy(dest_str,*(reg_codes + dest_r + 8));
       ea_code(sorc_str);
     }
   else if (dest_m >= 4 && ea_m <= 1)
/*
*    SUBX  Dy,Dx  -  SUBX  -(Ay),-(Ax)
*/
     {
       strcpy(instr_str,"SUBX");
       strcat(instr_str,*(size_codes + op_size));
       rxry_code(1,sorc_str,dest_str);
     }
   else
/*
*    SUB  <ea>,Dn  -  SUB  Dn,<ea>
*/
     {
       strcpy(instr_str,"SUB");
       strcat(instr_str,*(size_codes + op_size));
       strcpy(dest_str,*(reg_codes + dest_r));
       if (dest_m < 4)
         ea_code(sorc_str);
       else
         {
           strcpy(sorc_str,dest_str);
           addr_modes(MEM_ALTER);
	   ea_code(dest_str);
         }
     }
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_10    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Process line A instructions.
*/

void class_10 (void)
{
   char  sorc_str[38];
   int  temp;
   static char *vmeprom[] =
     {"XSWP","XSMP","XGMP","X881","XUSP","XPAD","XERR","XEXT",
      "XGML","XRTS","XLKT","XULT","XSEF","XTEF","XSUI","XGTM",
      "XSTM","XGTP","XDTV","XCTB","XKTM","XRDM","XSUP","XLSR",
      "XEXC","XDEV","XRTP","XUAD","XBUG","XLER","XSTP","XGUM",
      "XFUM","XRSR","XRTE","XSEV","XGCB","XDMP","XEXZ","XPCB",
      "XCBD","XCBH","XCBM","XCDB","XFTD","XGNP","XRDT","XRTM",
      "XUDT","XUTM","XWDT","XWTM","XCHX","XCBX","XAIM","XPEL",
      "XBCP","XCBC","XCBP","XCLS","XGCC","XGCR","XGLB","XGLM",
      "XGLU","XGLX","XPBC","XPCC","XPCL","XPLC","XPMC","XPSC",
      "XTAB","XRCP","XRPS","XPDC","XPSP","XSPF","XPEM","XGCP",
      "XFFN","XLFN","XLST","XRDE","XRDN","XAPF","XCHF","XCPY",
      "XLDF","XRCN","XRST","XSZF","XBFL","XPCR","XPCP","XBER",
      "XISE","XRSE","XRSZ","XWSE","    ","    ","    ","XFAC",
      "XCFA","XCLF","XDFL","XDLF","XLKF","XNOP","XPSF","XRBF",
      "XRFA","XRLF","XRNF","XROO","XROP","XRWF","XSOP","XULF",
      "XWBF","XWFA","XWLF","XZFL","XFBF","XKTB","XWFP","XRFP",
      "XIT0","XIT1","XIT2","XIT3","XIT4","XIT5","XIT6","XIT7",
      "XTLP","XSOE","XDPE","XVEC"
     };

   *sorc_str = '\0';
   if(instr_wd == 0xa054) disp_code(1,0,sorc_str);
   else if (instr_wd == 0xa08c) disp_code(1,0,sorc_str);
   else if (instr_wd == 0xa090)
          {
            op_size = 1;
            imm_code(sorc_str);
          }
   else if (instr_wd == 0xa09c) disp_code(1,0,sorc_str);
   else if (instr_wd >= 0xa0c8 && instr_wd <= 0xa0cc) instr_valid = FALSE;
   else if (instr_wd > 0xa116) instr_valid = FALSE;
   if((instr_wd & 1) != 0) instr_valid = FALSE;
   temp = instr_wd & 0x1ff;
   if(temp <= 0x116)
    {
      strcpy(instr_str,*(vmeprom + temp/2));
    }
   instr_finis(sorc_str,"");
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_11    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Process CMP, CMPA, EOR and CMPM instructions.
*/

void class_11 (void)
{
   char  sorc_str[38],dest_str[38];

   if (dest_m <= 2)
/*
*    CMP  <ea>,Dn
*/
     {
       strcpy(instr_str,"CMP");
       strcat(instr_str,size_codes[op_size]);
       strcpy(dest_str,reg_codes[dest_r]);
       ea_code(sorc_str);
     }
   else if (dest_m == 3 || dest_m == 7)
/*
*    CMPA  <ea>,An
*/
     {
       op_size = 1;
       if (dest_m == 7) op_size = 2;
       strcpy(instr_str,"CMPA");
       strcat(instr_str,*(size_codes + op_size));
       strcpy(dest_str,*(reg_codes + dest_r + 8));
       ea_code(sorc_str);
     }
   else if (dest_m >= 4)
/*
*    CMPM  (Ay)+,(Ax)+
*/
     {
       if (ea_m == 1)
	 {
	   strcpy(instr_str,"CMPM");
	   strcat(instr_str,*(size_codes + op_size));
	   rxry_code(0,sorc_str,dest_str);
	 }
       else
	 {
/*
*    EOR  Dn,<ea>
*/
	   strcpy(instr_str,"EOR");
	   strcat(instr_str,*(size_codes + op_size));
	   strcpy(sorc_str,*(reg_codes + dest_r));
	   addr_modes(DATA_ALTER);
           ea_code(dest_str);
	 }
     }
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_12    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Process ABCD, EXG, MULU.W and MULS.W instructions.
*/

void class_12 (void)
{
   char  sorc_str[38],dest_str[38];

   if (ea_m <= 1 && (dest_m >= 4 && dest_m <= 6))
/*
*    ABCD  Dy,Dx  -  ABCD  -(Ay),-(Ax)
*/
     {
       if (dest_m == 4)
	 {
	   strcpy(instr_str,"ABCD");
	   rxry_code(1,sorc_str,dest_str);
	 }
       else if (dest_m == 5 || dest_m == 6)
	 {
	   strcpy(instr_str,"EXG");
	   if (dest_m == 5)
	     {
/*
*    EXG  Dx,Dy  -  EXG  Ax,Ay
*/
	       if (ea_m == 1) {dest_r += 8; ea_r += 8;}
	     }
	   else if (dest_m == 6 && ea_m == 1)
/*
*    EXG  Ay,Dx
*/
               ea_r += 8;
	   else
	     instr_valid = FALSE;
	   strcpy(sorc_str,*(reg_codes + dest_r));
	   strcpy(dest_str,*(reg_codes + ea_r));
  	 }
     }
   else if (dest_m == 3 || dest_m == 7)
/*
*    MULU/MULS  <ea>,Dn
*/
     {
       strcpy(instr_str,"MULU.W");
       if (dest_m == 7) strcpy(instr_str,"MULS.W");
       strcpy(dest_str,*(reg_codes + dest_r));
       op_size = 1;
       addr_modes(DATA);
       ea_code(sorc_str);
     }
   else
     {
/*
*    AND  <ea>,Dn  -  AND  Dn,<ea>
*/
       strcpy(instr_str,"AND");
       strcat(instr_str,*(size_codes + op_size));
       strcpy(dest_str,*(reg_codes + dest_r));
       if (dest_m < 4)
         {
           addr_modes(DATA);
           ea_code(sorc_str);
         }
       else
         {
           strcpy(sorc_str,dest_str);
           addr_modes(MEM_ALTER);
           ea_code(dest_str);
         }
     }
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_13    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Process ADD, ADDA and ADDX instructions.
*/

void class_13 (void)
{
   char  sorc_str[38],dest_str[38];

   if (dest_m == 3)
/*
*    ADDA  <ea>,An
*/
     {
       strcpy(instr_str,"ADDA");
       op_size = 1;
       strcpy(dest_str,*(reg_codes + dest_r + 8));
       ea_code(sorc_str);
     }
   else if (dest_m == 7)
/*
*    ADDA.L  <ea>,An
*/
     {
       strcpy(instr_str,"ADDA.L");
       op_size = 2;
       strcpy(dest_str,*(reg_codes + dest_r + 8));
       ea_code(sorc_str);
     }
   else if (dest_m >= 4 && ea_m <= 1)
/*
*    ADDX  Dy,Dx  -  ADDX  -(Ay),-(Ax)
*/
     {
       strcpy(instr_str,"ADDX");
       strcat(instr_str,*(size_codes + op_size));
       rxry_code(1,sorc_str,dest_str);
     }
   else
/*
*    ADD  <ea>,Dn  -  ADD  Dn,<ea>
*/
     {
       strcpy(instr_str,"ADD");
       strcat(instr_str,*(size_codes + op_size));
       strcpy(dest_str,reg_codes[dest_r]);
       if (dest_m < 4)
         ea_code(sorc_str);
       else
         {
           strcpy(sorc_str,dest_str);
           addr_modes(MEM_ALTER);
	   ea_code(dest_str);
         }
     }
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_14    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/*
*   Process Shift/Rotate register or memory and Bit Field instructions.
*/

void class_14 (void)
{
   char  sorc_str[38],dest_str[38],tmp_str[38];
   char  off_str[18],wid_str[18];
   static char *sr_ins[] = {"ASR","ASL","LSR","LSL","ROXR","ROXL",
                            "ROR","ROL"};
   static char *bf_ins[] = {"BFTST","BFEXTU","BFCHG","BFEXTS",
                            "BFCLR","BFFFO","BFSET","BFINS"};
   int   tf,dx,wx,reg3,off,wid;
   long int temp;

   *dest_str = '\0';
   if (op_size <= 2)
/*
*    Shift instructions of the form  XXX  Dx,Dy  and  XXX #<count>,Dy
*/
     {
       tf = ea_m * 2 & 7;
       if ((instr_wd & 0x100)  != 0) tf += 1;
       strcpy(instr_str,sr_ins[tf]);
       strcat(instr_str,size_codes[op_size]);
       if ((instr_wd & 0x20) == 0)
/*
*    Dx field is the shift count
*/
         {
           if (dest_r == 0) dest_r = 8;
           strcpy(sorc_str,"# ");
	   sorc_str[1] = (char) (dest_r + 0x30);
         }
       else
/*
*    Dx contains shift count
*/
	 strcpy(sorc_str,*(reg_codes + dest_r));
       strcpy(dest_str,*(reg_codes + ea_r));
     }
   else
     {
       tf = op_cc & 7;
       if ((instr_wd & 0x800) == 0)
/*
*    Shift memory  XXX  <ea>
*/
         {
	   strcpy(instr_str,*(sr_ins + tf));
           addr_modes(MEM_ALTER);
	   ea_code(sorc_str);
           op_size = 0;
         }
       else
/*
*    Bit field instructions.  Operand forms are:  <ea>{offset:width},Dn
*    and  Dn,<ea>{offset:width} .  Offset and width fields are in the
*    extension word and may be the respective values or specify data
*    registers which contain the values.
*
*  MC68020 up
*/
        {
           if (cpu < 2) instr_valid = FALSE;
           if (ea_m != 0) addr_modes(CNTL_ALTER);
	   strcpy(instr_str,*(bf_ins + tf));
           temp = get_dat(2);
	   reg3 = (int) (temp >> 12);
           if (reg3 > 7) instr_valid = FALSE;
           dx = 0;
           wx = 0;
           if ((temp & 0x0800) != 0) dx = 1;
           if ((temp & 0x0020) != 0) wx = 1;
	   off = (int) ((temp >> 6) & 0x1F);
	   wid = (int) (temp & 0x1F);
           if (dx == 1 && off > 7 || wx == 1 && wid > 7)
                                          instr_valid = FALSE;
           else
             {
               if (dx == 1)
/*
*   Offset field specifies a data register
*/
		 strcpy(off_str,*(reg_codes + off));
               else
                 {
		   num_code((long)off,tmp_str);
		   strcpy(off_str,"$");
		   strcat(off_str,tmp_str + 6);
                 }
               if (wx == 1)
/*
*   Width field specifies a data register
*/
		 strcpy(wid_str,*(reg_codes + wid));
               else
                 {
		   num_code((long)wid,tmp_str);
		   strcpy(wid_str,"$");
		   strcat(wid_str,tmp_str + 6);
                 }
               ea_code(sorc_str);
               strcat(sorc_str,"{");
               strcat(sorc_str,off_str);
               strcat(sorc_str,":");
               strcat(sorc_str,wid_str);
               strcat(sorc_str,"}");
               if ((tf & 1) == 0)
                 {
                   if (reg3 != 0) instr_valid = FALSE;
                 }
               else
/*
*   Register field specifies the destination register for all except
*   BFINS.  For BFINS it is the source.
*/
                 {
		   strcpy(tmp_str,*(reg_codes + reg3));
                   if (tf == 7)
                     {
/*
*   BFINS instruction.
*/
                       strcpy(dest_str,sorc_str);
                       strcpy(sorc_str,tmp_str);
                     }
                   else
                     strcpy(dest_str,tmp_str);
                 }
             }
         }
     }
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     class_15    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*
*    Process line F instructions.
*/


void class_15 (void)
{
   char  sorc_str[38],dest_str[38];

   *dest_str = '\0';
/*
*   Dispatch on Coprocessor ID
*/
   if (dest_r == 0 && cpu == 3) mmu30(sorc_str,dest_str);  /* MC68030 */
   else if (dest_r == 1)
     {
/*
*    MC68881/MC68882 floating-point coprocessor
*
*    Check for disassembly enabled.
*/
       if (fpcp == 0 && cpu != 4) instr_valid = FALSE;
       if (dest_m == 0)
	   fpgen(sorc_str,dest_str);
       else if (dest_m == 1)
	   fpdbcc(sorc_str,dest_str);
       else if (dest_m == 2 || dest_m == 3)
	   fpbcc(sorc_str,dest_str);
       else if (dest_m == 4)
         {
           strcpy(instr_str,"FSAVE");
           if (ea_m != 4) addr_modes(CNTL_ALTER);
           ea_code(sorc_str);
         }
       else if (dest_m == 5)
         {
           strcpy(instr_str,"FRESTORE");
           if (ea_m != 3) addr_modes(CNTL_ALTER);
           ea_code(sorc_str);
         }
       else
	 instr_valid = FALSE;
     }
   else if (dest_r == 2 && cpu == 4) mmu40(sorc_str,dest_str);  /* MC68040 */
   else if (dest_r == 3 && cpu == 4) move16(sorc_str,dest_str); /* MC68040 */
   else
      instr_valid = FALSE;
   instr_finis(sorc_str,dest_str);
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$     move16     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*
*   Encode MOVE16 instruction.  MC68040 only.
*/
void move16 (char *sorc_str,char *dest_str)
{
   long int temp;

   if (dest_m != 0 || ea_m > 4) instr_valid = FALSE;
   else
     {
       strcpy(instr_str,"MOVE16");
       if (ea_m == 4)
        {
          ea_m = 3;
          ea_code(sorc_str);
          temp = get_dat(2);
          if ((temp & 0x8fff) != 0x8000) instr_valid = FALSE;
          temp = temp >> 12;
          ea_r = temp & 7;
          ea_code(dest_str);
        }
       else if (ea_m == 0 || ea_m == 2)
        {
          if (ea_m == 0) ea_m = 3;
          else  ea_m = 2;
          ea_code(sorc_str);
          ea_m = 7;
          ea_r = 1;
          ea_code(dest_str);
        }
       else
        {
          if (ea_m == 1) ea_m = 3;
          else  ea_m = 2;
          ea_code(dest_str);
          ea_m = 7;
          ea_r = 1;
          ea_code(sorc_str);
        }
     }
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$    mmu30_fc     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*
*   Encode the FC field for 68030 memory management instructions.
*/
void mmu30_fc (int fc,char *sorc_str)
{
   if (fc == 0) strcpy(sorc_str,"SFC");
   else if (fc == 1) strcpy(sorc_str,"DFC");
   else if (fc >= 8 && fc <= 0xf)
     {
       ea_m = 0;
       ea_r = fc & 7;
       ea_code(sorc_str);
     }
   else if (fc >= 0x10 && fc <= 0x17)
     {
       *sorc_str = '#';
       *(sorc_str + 1) = '0' + (fc & 7);
       *(sorc_str + 2) = '\0';
     }
   else  instr_valid = FALSE;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$    mmu30_ea     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*
*   Encode the effective address for 68030 memory management instructions.
*/
void mmu30_ea (long int temp,char *sorc_str,char *dest_str)
{
   addr_modes(CNTL_ALTER);
   if ((temp & 0x200) != 0)
     {
      ea_code(dest_str);
     }
   else
     {
       strcpy(dest_str,sorc_str);
       ea_code(sorc_str);
     }
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$      mmu30      $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*
*   Memory management for MC68030.
*/
void mmu30 (char *sorc_str,char *dest_str)
{
   long int temp;
   int mode,mask,fc,type;
   char tmp_str[12];

   if (dest_m == 0)
    {
      temp = get_dat(2);
      type = (temp & 0xe000) >> 13;
      mode = (temp & 0x1c00) >> 10;
      mask = (temp & 0xe0) >> 5;
      fc = temp & 0x1f;
      if (type == 0)
        {
          if ((temp & 0xff) != 0) instr_valid = FALSE;
          strcpy(instr_str,"PMOVE");
          if ((temp & 0x100) != 0) strcat(instr_str,"FD");
          strcat(instr_str,".L");
          if (mode == 2) strcpy(sorc_str,"TT0");
          else if (mode == 3) strcpy(sorc_str,"TT1");
          else  instr_valid = FALSE;
          mmu30_ea(temp,sorc_str,dest_str);
        }
      else if (type == 2)
        {
          if ((temp & 0xff) != 0) instr_valid = FALSE;
          strcpy(instr_str,"PMOVE");
          if ((temp & 0x100) != 0) strcat(instr_str,"FD");
          if (mode == 0) strcpy(sorc_str,"TC");
          else if (mode == 2) strcpy(sorc_str,"SRP");
          else if (mode == 3) strcpy(sorc_str,"CRP");
          else  instr_valid = FALSE;
          if (mode < 2) strcat(instr_str,".L");
          else  strcat(instr_str,".D");
          mmu30_ea(temp,sorc_str,dest_str);
        }
      else if (type == 1)
        {
          if (mode == 0)
           {
             strcpy(instr_str,"PLOAD");
             if ((temp & 0x200) == 0) strcat(instr_str,"W");
             else  strcat(instr_str,"R");
             mmu30_fc(fc,sorc_str);
             addr_modes(CNTL_ALTER);
             ea_code(dest_str);
           }
          else
           {
             if ((temp & 0x300) != 0) instr_valid = FALSE;
             strcpy(instr_str,"PFLUSH");
             *sorc_str = '\0';
             if (mode == 1)
              {
               strcat(instr_str,"A");
               if (mask != 0 || fc != 0) instr_valid = FALSE;
               if ((instr_wd &0xff) != 0) instr_valid = FALSE;
              }
             else if (mode == 4 || mode == 6)
              {
               if (mode == 6)
                {
                 addr_modes(CNTL_ALTER);
                 ea_code(dest_str);
                }
               else
                {
                  if ((instr_wd &0xff) != 0) instr_valid = FALSE;
                }
               mmu30_fc(fc,sorc_str);
               strcat(sorc_str,",");
               *tmp_str = '#';
               *(tmp_str + 1) = '0' + mask;
               *(tmp_str + 2) = '\0';
               strcat(sorc_str,tmp_str);
              }
             else  instr_valid = FALSE;
           }
        }
      else if (type == 3)
        {
          if ((temp & 0x1cff) != 0) instr_valid = FALSE;
          strcpy(instr_str,"PMOVE.W");
          strcpy(sorc_str,"MMUSR");
          mmu30_ea(temp,sorc_str,dest_str);
        }
      else if (type == 4)
        {
          strcpy(instr_str,"PTEST");
          if ((temp &  0x200) == 0) strcat(instr_str,"W");
          else  strcat(instr_str,"R");
          addr_modes(CNTL_ALTER);
          ea_code(dest_str);
          mmu30_fc(fc,sorc_str);
          strcat(sorc_str,",");
          strcat(sorc_str,dest_str);
          *dest_str = '#';
          *(dest_str + 1) = '0' + mode;
          *(dest_str + 2) = '\0';
          if ((temp & 0x100) != 0)
            {
              ea_m = 1;
              ea_r = mask;
              ea_code(tmp_str);
              strcat(dest_str,",");
              strcat(dest_str,tmp_str);
            }
          else if (mask != 0) instr_valid = FALSE;
        }
      else  instr_valid = FALSE;
    }
   else  instr_valid = FALSE;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$      mmu40      $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*
*   Memory management for MC68040.
*/
void mmu40 (char *sorc_str,char *dest_str)
{
   if (dest_m == 4)
     {
       *sorc_str = '\0';
       strcpy(instr_str,"PFLUSH");
       if (ea_m == 0 || ea_m == 1)
         {
           if (ea_m == 1) strcat(instr_str,"N");
           ea_m = 2;
           ea_code(sorc_str);
         }
        else if (ea_m == 2) strcat(instr_str,"AN");
        else if (ea_m == 3) strcat(instr_str,"A");
        else  instr_valid = FALSE;
     }
   else if (dest_m == 5)
     {
       strcpy(instr_str,"PTEST");
       if (ea_m == 1) strcat(instr_str,"W");
       else if (ea_m == 5) strcat(instr_str,"R");
       else  instr_valid = FALSE;
       ea_m = 2;
       ea_code(sorc_str);
     }
   else if (dest_m >= 0 && dest_m <= 3)
     {
       if (ea_m < 4) strcpy(instr_str,"CINV");
       else 
         {
           strcpy(instr_str,"CPUSH");
           ea_m = ea_m & 3;
         }
       *sorc_str = '#';
       *(sorc_str + 1) = '0' + dest_m;
       *(sorc_str + 2) = '\0';
       if (ea_m == 0) instr_valid = FALSE;
       else if (ea_m == 3) strcat(instr_str,"A");
       else if (ea_m > 3) instr_valid = FALSE;
       else
         {
           if (ea_m == 1) strcat(instr_str,"L");
           else  strcat(instr_str,"P");
           ea_m = 2;
           ea_code(dest_str);
         }
     }
   else  instr_valid = FALSE;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$       fpgen     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

void fpgen (char *sorc_str,char *dest_str)
{
   static int  opsizes[8] = {2,2,12,12,1,8,0,12};
   static char *opcode[64] = 
                {"FMOVE","FINT","FSINH","FINTRZ","FSQRT","","FLOGNP1","",
                "FETOXM1","FTANH","FATAN","","FASIN","FATANH","FSIN","FTAN",
                "FETOX","FTWOTOX","FTENTOX","","FLOGN","FLOG10","FLOG2","",
                "FABS","FCOSH","FNEG"," ","FACOS","FCOS","FGETEXP","FGETMAN",
                "FDIV","FMOD","FADD","FMUL","FSGLDIV","FREM","FSCALE","FSGLMUL",
                "FSUB","","","","","","","",
                "FSINCOS","FSINCOS","FSINCOS","FSINCOS",
                "FSINCOS","FSINCOS","FSINCOS","FSINCOS",
                "FCMP","","FTST","","","","",""};
   static char *ctl_regs[8] =
                { "","FPIAR","FPSR","FPSR/FPIAR","FPCR","FPCR/FPIAR",
                "FPCR/FPSR","FPCR/FPSR/FPIAR"};

   long int temp;
   int      ext,rx,ry,opclass;
   char     tmp_str[38];

   *dest_str = '\0';
   temp = get_dat(2);
   ext = (int) (temp & 0x7F);
   opclass = (int) ((temp >> 13) & 7);
   rx = (int) ((temp >> 10) & 7);
   ry = (int) ((temp >> 7) & 7);
   if (opclass == 0)
     {
/*
*    FP data register to FP data register operations
*/
       strcpy(sorc_str,*(fpreg_codes + rx));
       strcpy(dest_str,*(fpreg_codes + ry));
       *instr_str = '\0';
       if (ext < 64) strcpy(instr_str,*(opcode + ext));
       if (*instr_str == '\0') instr_valid = FALSE;
       strcat(instr_str,".X");
       if (ext <= 31)
         {
           if (rx == ry) *dest_str = '\0';
         }
       else if (ext == 0x3A)
         {
           *dest_str = '\0';
           if (ry != 0) instr_valid = FALSE;
         }
       else if (ext >= 0x30 && ext <= 0x37)
         {
           strcpy(tmp_str,dest_str);
           strcpy(dest_str,fpreg_codes[temp & 7]);
           strcat(dest_str,":");
           strcat(dest_str,tmp_str);
         }
       if ((instr_wd & 0x1FF) != 0) instr_valid = FALSE;
     }
   else if (opclass == 1)
       instr_valid = FALSE;
   else if (opclass == 2)
     {
/*
*    External operand to FP data register
*/
       if (rx == 7)
/*
*    ROM constant
*/
         {
           strcpy(instr_str,"FMOVECR");
	   strcpy(dest_str,*(fpreg_codes + ry));
	   num_code((long)ext,tmp_str);
           strcpy(sorc_str,"#$");
	   strcat(sorc_str,tmp_str + 6);
           if ((instr_wd & 0x1FF) != 0) instr_valid = FALSE;
         }
       else
         {
/*
*    <ea> Source
*/
	   strcpy(dest_str,*(fpreg_codes + ry));
           *instr_str = '\0';
	   if (ext < 64) strcpy(instr_str,*(opcode + ext));
           if (*instr_str == '\0') instr_valid = FALSE;
	   strcat(instr_str,*(fpsize_codes + rx));
           if (ext == 0x3a)
             {
               *dest_str = '\0';
               if (ry != 0) instr_valid = FALSE;
             }
	   else if (ext >= 0x30 && ext <= 0x37)
             {
               strcpy(tmp_str,dest_str);
	       strcpy(dest_str,*(fpreg_codes + (temp & 7)));
               strcat(dest_str,":");
               strcat(dest_str,tmp_str);
             }
	   op_size = *(opsizes + rx);
           if (rx <= 1 || rx == 4 || rx == 6)
             addr_modes(DATA);
           else
             {
               addr_modes(MEM);
               if (ea_m == 0) instr_valid = FALSE;
             }
           ea_code(sorc_str);
         }
     }
   else if (opclass == 3)
     {
/*
*   FP data register to external destinatiion
*/
       strcpy(instr_str,"FMOVE");
       strcat(instr_str,*(fpsize_codes + rx));
       strcpy(sorc_str,*(fpreg_codes + ry));
       if (ea_m == 0 && *(opsizes + rx) > 4) instr_valid = FALSE;
       if (rx <= 1 || rx == 4 || rx == 6)
         addr_modes(DATA_ALTER);
       else
         addr_modes(MEM_ALTER);
       ea_code(dest_str);
       if (rx == 4)
         {
	   num_code((long)ext,tmp_str);
           strcat(dest_str,"[$");
	   strcat(dest_str,tmp_str + 6);
           strcat(dest_str,"]");
         }
       else if (rx == 7)
         {
           if ((ext & 0xF) != 0) instr_valid = FALSE;
           strcat(dest_str,"[");
	   strcat(dest_str,*(reg_codes + (ext/16)));
           strcat(dest_str,"]");
         }
     }
   else if (opclass == 4 || opclass == 5)
     {
/*
*    FP Control register(s) moves
*/
       strcpy(instr_str,"FMOVE.L");
       if (rx != 1 && rx != 2 && rx != 4)
         {
           strcpy(instr_str,"FMOVEM.L");
           if (ea_m <= 1) instr_valid = FALSE;
         }
       else  op_size = 2;
       if (ea_m == 1 && rx != 1) instr_valid = FALSE;
       if ((temp & 0x3FF) != 0) instr_valid = FALSE;
       if (rx == 0) instr_valid = FALSE;
       if (opclass == 4)
         {
	   strcpy(dest_str,*(ctl_regs + rx));
           ea_code(sorc_str);
         }
       else
         {
	   strcpy(sorc_str,*(ctl_regs + rx));
           addr_modes(ALTER);
           ea_code(dest_str);
         }
     }
   else if (opclass >= 6)
     {
/*
*   FP multiple data register moves
*/
        strcpy(instr_str,"FMOVEM.X");
        if ((temp & 0x700) != 0) instr_valid = FALSE;
        rx = rx/2;
        if (rx == 0 || rx == 2)
	  fpreg_list((int) (temp & 0xFF),tmp_str);
        else
          {
            if ((temp & 0x8F) != 0) instr_valid = FALSE;
	    strcpy(tmp_str,*(reg_codes + (temp/16 & 7)));
          }
        if (opclass == 6)
          {
            strcpy(dest_str,tmp_str);
            if (ea_m != 3) addr_modes(CNTL);
            ea_code(sorc_str);
          }
        else
          {
            strcpy(sorc_str,tmp_str);
            if (ea_m != 4) addr_modes(CNTL_ALTER);
            ea_code(dest_str);
          }
     }
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$    fpreg_list   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

void fpreg_list (int test,char *str)
{
   int   creg = 0;   /*  current register      */
   int   lastr = 0;  /*  last register in list */
   int   lastb = 0;  /*  last bit test value   */
   int   msk = 0x80; /*  bit test mask         */
   int   sfd = -1;   /*  shift direction. positive = left, negative = right*/
   char  *ptr;

   ptr = str;
   if (ea_m == 4) {sfd = -sfd;  msk = 1;}

   while (creg < 8)
     {
       if ((test & msk) == 0)
         {
           if (lastb == 1)
             if (creg - lastr > 1)
               {
                 *ptr++ = '-';
		 strcpy(ptr,*(fpreg_codes + creg-1));
                 ptr += 3;
               }
           lastb = 0;
         }
       else
         {
           if (lastb == 0)
             {
               if (ptr != str) {*ptr = '/'; ++ptr;}
	       strcpy(ptr,*(fpreg_codes + creg));
               ptr = ptr + 3;
               lastr = creg;
             }
           lastb = 1;
         }
       if (sfd > 0)
          msk <<= 1;
       else
          msk >>= 1;
       creg += 1;
     }
   *ptr = '\0';
   if (lastb == 1 && lastr != 7) strcat(ptr,"-FP7");
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$       fpdbcc    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

void fpdbcc (char *sorc_str,char *dest_str)
{
   long int temp,disp;

   *dest_str = '\0';
   temp = get_dat(2);
   if ((temp & 0xFFE0) != 0) instr_valid = FALSE;
   temp = temp & 0x1F;
   if (ea_m == 1)
     {
/*
*    FDBcc  Dn,<label>
*/
       strcpy(instr_str,"FDB");
       if (temp != 0)
         strcat(instr_str,fpcc_codes[temp]);
       else
         strcat(instr_str,"RA");
       strcpy(sorc_str,*(reg_codes + ea_r));
       disp = disp_code(1,0,dest_str);
       if (disp > -2 && disp < ir_len - 2) instr_valid = FALSE;
       if ((disp & 1) != 0) instr_valid = FALSE;
     }
   else if (ea_m == 7)
     {
/*
*    FTRAPcc  instructions
*/
       strcpy(instr_str,"FTRAP");
       strcat(instr_str,fpcc_codes[temp]);
       if (ea_r == 4)
/*
*    FTRAPcc with no parameter
*/
         *sorc_str = '\0';
       else if (ea_r == 2)
/*
*    FTRAPcc with WORD parameter
*/
         {
           op_size = 1;
           imm_code(sorc_str);
         }
       else if (ea_r == 3)
         {
/*
*    FTRAPcc with LONG parameter
*/
           op_size = 2;
           imm_code(sorc_str);
           strcat(instr_str,".L");
         }
       else
         instr_valid = FALSE;
     }
   else
     {
/*
*    FScc  <ea>
*/
       strcpy(instr_str,"FS");
       strcat(instr_str,fpcc_codes[temp]);
       addr_modes(DATA_ALTER);
       ea_code(sorc_str);
     }
   return;
}
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$       fpbcc     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

void fpbcc (char *sorc_str,char *dest_str)
{
   long int temp,disp;

   *dest_str = '\0';
   temp = instr_wd & 0x1F;
   if ((instr_wd & 0x20) != 0) instr_valid = FALSE;
/*
*    FBcc  <label>
*/
   disp = disp_code(dest_m - 1,0,sorc_str);
   if (temp == 0 && dest_m == 2 && disp == 0)
     {
/*
*   FNOP instruction.
*/
       strcpy(instr_str,"FNOP");
       *sorc_str = '\0';
     }
   else
     {
       strcpy(instr_str,"FB");
       strcat(instr_str,*(fpcc_codes + temp));
       if (disp > -2 && disp < ir_len - 2) instr_valid = FALSE;
       if ((disp & 1) != 0) instr_valid = FALSE;
     }
   return;
}
