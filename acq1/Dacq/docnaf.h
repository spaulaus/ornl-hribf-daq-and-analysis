/*  cnaf.h    */
/*  Include definitions widely useful to the cnaf server and client */

#define MAXCNAF 181

struct cnafd {
       char C;
       char N;
       char A;
       char F;
       int Data;
} ;
struct cnaf_buffer {
       int count;
       struct cnafd cnafs[MAXCNAF];
} ;
/*   The return must have X, Q, and Data (non-zero where read) */
   struct cnaf_return {
        char X;   /* CAMAC X status */
        char Q;   /* CAMAC Q status */
        char Cok; /* Cok=0: no such crate; =1 crate exists */
        char Online; /* =0 crate offline; =1 crate online */
        int Data;
   }  ;

/*   Function to compute the byte length of the cnaf_buffer  */
int cnaflen (struct cnaf_buffer *buff)
{
    return (buff->count*sizeof(struct cnafd)+sizeof(int));
}

/*   Function to compute number of CNAFs in the buffer  */
int cnafcnt (int bufflen)
{
    return ((bufflen - sizeof(int))/sizeof(struct cnafd));
}
