/*
*   Parameters which define the entry points for LeCroy 1821 SM/I
*   macros.   Sequencer code  seqcode.v31
*/
#define   DSR       0x03  /*Primary address to Data Space               */
#define   CSR       0x07  /*Primary address to Control Space            */
#define   WR        0x11  /*Write FASTBUS module from 32 bit reg        */
#define   RD        0x0b  /*Read FASTBUS module to 32 bit reg and pipe  */
#define   RD_NTA    0x0e  /*Read secondary address to 32 bit reg        */
#define   WR_NTA    0x14  /*Write secondary address from 32 bit reg     */
#define   RD_BLK    0x17  /*Read and pipe till SS = 2                   */
#define   ZERO32    0x1a  /*Clear 32 bit reg                            */
#define   WR_TCNTR  0x45  /*Write TCNTR                                 */
#define   RD_TCNTR  0x47  /*Read TCNTR                                  */
#define   WR_NREG   0x49  /*Write NREG                                  */
#define   RD_NREG   0x4b  /*Read NREG                                   */
#define   WR32_L    0x4d  /*Write 16 LSB of 32 bit reg                  */
#define   WR32_H    0x50  /*Write 16 MSB of 32 bit reg                  */
#define   RD_BYTE0  0x53  /*Read byte 0 (LSB) of 32 bit reg             */
#define   RD_BYTE1  0x55
#define   RD_BYTE2  0x57
#define   RD_BYTE3  0x59  /*Read byte 3 (MSB) of 32 bit reg             */
#define   REBUS     0x5b  /*Reset FASTBUS                               */
#define   READ_ADC  0x70
#define   READ_TDC  0x90
