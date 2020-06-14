/* JRB 6/90 
   RLV 5/92 - use shared memory
SOURCE FILE:  mem_fort.c   
LIBRARY:      jblibc1.a
-------------------------------------------------------------------
   Routines for RUNTIME Hisogram Memory Allocation and Manipulation.

   These fortran callable c routines are designed for implementation of
   runtime memory allocation in SCANM type programs.

   In the following, the memory allocated at runtime is refered 
   to as hisspace.

   Note that only the routines in this package have direct access to
   the memory allocated to histogram storage, so calls to this package 
   must be made for access to channel values, incrementing, and io.

   Two general memory manipulation utilities SETMEMORY and
   COPYMEMORY are included.

Use INTEGER*4 for ALL arguments and you can't go wrong.
---------------------------------------------------------------------
SUBROUTINE SHM_GET_HISSPACE(NBYTES,IERR)
Requests NBYTES of shared memory. IERR=0 if successful
---------------------------------------------------------------------
SUBROUTINE SHM_OPEN(SHMID,IERR)
Actually assigns the address.  IERR=0 if OK.
---------------------------------------------------------------------
SUBROUTINE SHM_CLOSE(SHMID,IERR)
Deassigns the address.  IERR=0 if OK.
---------------------------------------------------------------------
SUBROUTINE SHM_DELETE(SHMID,IERR)
Actually assigns the address.  IERR=0 if OK.
---------------------------------------------------------------------
SUBROUTINE MEM_ZOT_HW(START_WORD, END_WORD)
Set the specified region of histogram memory to zero. (Half-Word)
---------------------------------------------------------------------
SUBROUTINE MEM_ZOT_FW(START_WORD, END_WORD)
Set the specified region of histogram memory to zero. (Full-Word)
---------------------------------------------------------------------
SUBROUTINE MEM_READ_HW(BUFF, NWN, NHWD)
Reads NHWD half words of memory, starting at NWN into BUFF.
---------------------------------------------------------------------
SUBROUTINE MEM_GET_HISSPACE(NBYTES,IERR)
Requests NBYTES of memory. IERR=0 if successful
---------------------------------------------------------------------
SUBROUTINE MEM_ZERO_HISSPACE(NBYTES)
Zeroes NBYTES of hisspace from the beginning.
---------------------------------------------------------------------
SUBROUTINE MEM_ADD1_FW(IADDR)          
Adds 1 to memory location corresponding to IADDR, treating
hisspace as a full word array.
---------------------------------------------------------------------
SUBROUTINE MEM_ADD1_HW(IADDR)
Adds 1 to memory location corresponding to IADDR, treating
hisspace as a half word array.
---------------------------------------------------------------------
INTEGER FUNCTION MEM_GET_VALUE_FW(IADDR)
Gets value of IADDRth element of hisspace, treating
hisspace as a full word array.
---------------------------------------------------------------------
INTEGER*2 FUNCTION MEM_GET_VALUE_HW(IADDR)    
Gets value of IADDRth element of hisspace, treating
hisspace as a half word array.
---------------------------------------------------------------------
SUBROUTINE MEM_BUFI(LU,IBUF,IREC,NBY,IERR)
Does IO ops via hisspace, just like BUFI/BUFO for array.
---------------------------------------------------------------------
SUBROUTINE MEM_BUFO(LU,IBUF,IREC,NBY,IERR) 
Does IO ops via hisspace, just like BUFI/BUFO for array.
---------------------------------------------------------------------
 some general utilities
---------------------------------------------------------------------
SUBROUTINE SETMEMORY(IADDR,VALUE,NBYTES) 
Sets NBYTES of memory to the value in the low order byte of VALUE 
starting at IADDR.
---------------------------------------------------------------------
SUBROUTINE COPYMEMORY(DEST,SOURCE,NBYTES) 
Moves NBYTES of data from SOURCE to DEST
---------------------------------------------------------------------

   Only the routines in this file have direct access to the memory 
   allocated to histogram storage, so calls to this package must be 
   made for access to channel values, incrementing, and io.
*/
/*    This definitions apply to the entire file  */
#include <memory.h>
#include <string.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

static int *hisspace;
static int hissize=0;

extern void bufi_(int *, int *, int *, int *, int *);
extern void bufo_(int *, int *, int *, int *, int *);

/*  SUBROUTINE SHM_GET_HISSPACE(NBYTES,IERR)
    IERR=0   OK
    IERR=-1  FAILED
    Modified to allocate from shared memory rather than local.
*/
void shm_get_hisspace_(int *byte, int *shmid, int *ierr){
    unsigned int size;
    size= ((unsigned) *byte) + 4;

    /*   Create the segment structure Not advertised*/
    *shmid = shmget(IPC_PRIVATE, size, IPC_CREAT | 0666); 
    if (*shmid == -1) {
       perror("\nshm_get_hisspace");
       hissize=0;
       *ierr = -1;
       return;
    }
    else {
       *ierr = 0;
       hissize=size+4;
    }
}   
/*
    SUBROUTINE SHM_OPEN(SHMID, IERR)
    IERR = 0, OK; IERR = -1; Memory not attached
    This attaches the memory to a local address
    Must NOT assume that shm_get_hisspace has been called.
*/
void shm_open_(int *shmid, int *ierr)
{
    struct shmid_ds shm_data;

/*     Get the size and other facts about the segment  */
    if ((shmctl(*shmid, IPC_STAT, &shm_data)) == -1) {
       perror("\nshm_open - get info");
       *ierr = -1;
       return;
    }

    hisspace=(int *)shmat(*shmid, 0, 0);   /* Actually map the storage  */
    if((int) hisspace == -1){
       perror("\nshm_open_");
       hissize = 0;
       *ierr = -1;
       return;
    }
    else {
       hissize = shm_data.shm_segsz + 4;  /* Get size of existing segment */
       *ierr = 0;
       return;
    }
}
/*
    SUBROUTINE SHM_CLOSE(SHMID, IERR)
    IERR = 0, OK; IERR = -1; Memory not attached
    This detaches the shared memory from a local address
*/
void shm_close_(int *shmid, int *ierr)
{
    if ((shmdt((char *)hisspace)) == -1) {      /* Unmap the storage  */
       perror("\nshm_close_");
       *ierr = -1;
       return;
    }
    hissize=0;
    hisspace=0;
    *shmid=0;
    *ierr=0;
}
/*
    SUBROUTINE MEM_ZOT_HW(START_WORD, END_WORD)
    Set the specified region of histogram memory to zero.
    Assumes Half-Word starting point and end point.
*/
void mem_zot_hw_(int *StartWord, int *EndWord)
{
    short int *AddrPtr;
    int  count;

    count = 2 * (*EndWord - *StartWord + 1);
    if (count < 0 || count > hissize) return;
    AddrPtr = (short int *)hisspace;
    AddrPtr += (*StartWord - 1);
    memset(AddrPtr, 0, count);
}
/*
    SUBROUTINE MEM_ZOT_FW(START_WORD, END_WORD)
    Set the specified region of histogram memory to zero.
    Assumes Full-Word starting point and and end point.
*/
void mem_zot_fw_(int *StartWord, int *EndWord)
{
    int *AddrPtr;
    int  count;

    count = 4 * (*EndWord - *StartWord + 1);
    if (count < 0 || count > hissize) return;
    AddrPtr = (int *)hisspace;
    AddrPtr += (*StartWord - 1);
    memset(AddrPtr, 0, count);
}
/*
    SUBROUTINE MEM_READ_HW(BUFF, START_WORD, COUNT_WORD)
    "Reads" histograms from memory storage for DAMM 
    Assumes Half-Word starting point and count.
*/
void mem_read_hw_(char *buf, int *StartWord, int *CountWord)
{
    short int *AddrPtr;

    AddrPtr = (short int *)hisspace;
    AddrPtr += (*StartWord - 1);
       /*  Use fast copy */
    memcpy(buf, AddrPtr, 2*(*CountWord));
}
/*
    SUBROUTINE MEM_READ_BYTE(BUFF, START_BYTE, COUNT_BYTE)
    "Reads" histograms from memory storage for DAMM 
    Assumes byte starting point and count.
*/
void mem_read_byte_(char *buf, int *StartByte, int *CountByte)
{
    char *AddrPtr;

    AddrPtr = (char *)hisspace;
    AddrPtr += (*StartByte - 1);
       /*  Use fast copy */
    memcpy(buf, AddrPtr, *CountByte);
}
/*  SUBROUTINE SHM_DELETE(SHMID, IERR)
    Deletes a shared memory section.  Otherwise they linger around...
*/
void shm_delete_(int *shmid, int *ierr) {
    int status;
    status = shmctl(*shmid, IPC_RMID, NULL);
    if (status == -1) {
       perror("\nshm_delete");
       *ierr = -1;
    }
    else 
       *ierr = 0;
}
/*  SUBROUTINE MEM_GET_HISSPACE(NBYTES,IERR)
    IERR=0   OK
    IERR=-1  FAILED
*/
void mem_get_hisspace_(int *byte, int *ierr){
    unsigned size;
    size= ((unsigned) *byte) + 4;
    hisspace= (int *)malloc(size);
    if(hisspace==0){
       hissize=0;
       *ierr = -1;
    }
    else{
       *ierr = 0;
       hissize=size+4;
    }
}   
/*  SUBROUTINE MEM_ZERO_HISSPACE(IBYTES)
    zero entire hisspace memory block.
*/
void mem_zero_hisspace_(int *bytes){
    int nb;
    int c=0;
    if(*bytes < 0)
       nb=hissize;
    else if (hissize == 0)
       return;
    else if (hissize < *bytes)
       nb=hissize;
    else
       nb = *bytes;
    memset(hisspace,c,nb);
}
/*  SUBROUTINE MEM_ADD1_FW(IADDR)
    add 1 to the IADDRth full word in hissspace.
*/
void mem_add1_fw_(int *addr){
    int *next;
    next = hisspace;
    next+=(*addr-1);
    *next = *next + 1;
}
/*  SUBROUTINE MEM_ADD1_HW(IADDR)
    add 1 to the IADDRth half word in hissspace.
*/
void mem_add1_hw_(int *addr){
    short int *next;
    next = (short int *)hisspace;
    next+=(*addr-1);
    *next = *next + 1;
}
/*  INTEGER FUNCTION MEM_GET_VALUE_FW(IADDR)
    get the value of the IAADRth half word in hisspace.
*/
int mem_get_value_fw_(int *addr){
    int *next;
    next = hisspace;
    next+=(*addr-1);
    return *next;
}
/*  INTEGER FUNCTION MEM_GET_VALUE_HW(IADDR)
    get the value of the IAADRth half word in hisspace.
*/
short int mem_get_value_hw_(int *addr){
    short int *next;
    next = (short int *)hisspace;
    next+=(*addr-1);
    return *next; 
}
/*  SUBROUTINE MEM_BUFI(LU,IBUF,IREC,NBY,IERR)
    io access to hisspace. IBUF is strictly a dummy, included in 
    the parameter list for compatibility with the standard BUFI.
*/
void mem_bufi_(int *lut, int *ibuf, int *irec, int *nby, int *ierr){
     bufi_(lut, hisspace, irec, nby, ierr);
}
/*  SUBROUTINE MEM_BUFO(LU,IBUF,IREC,NBY,IERR)
    io access to hisspace. IBUF is strictly a dummy, included in 
    the parameter list for compatibility with the standard BUFO.
*/
void mem_bufo_(int *lut, int *ibuf, int *irec, int *nby, int *ierr){
     bufo_(lut, hisspace, irec, nby, ierr);
}
/*  SUBROUTINE SETMEMORY(IADDR,VALUE,NBYTES)
    set nbytes memory block to value of low order byte of value.
*/
void setmemory_(void *addr, int *value, int *bytes){
    size_t n;
    int c;
    c = *value;
    n = *bytes;
    memset(addr,c,n);
}
/*  SUBROUTINE COPYMEMORY(DEST,SOURCE,NBYTES)
    set nbytes memory block to value of low order byte of value.
*/
void copymemory_(void *s2, void *s1, int *bytes){
    size_t n;
    n = *bytes;
    memcpy(s2,s1,n);
}
