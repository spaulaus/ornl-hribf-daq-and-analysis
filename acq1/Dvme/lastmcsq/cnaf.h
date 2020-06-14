#ifndef  CNAF_H_
#define  CNAF_H_
/*  cnaf.h    */
/*  Include definitions widely useful to the cnaf server and client */

struct cnafd {
       char C;
       char N;
       char A;
       char F;
       int Data;
} ;

#define MAX_CNAF  182

struct cnaf_buffer {
       int count;
       struct cnafd cnafs[MAX_CNAF];
} ;
/*   The return must have X, Q, and Data (non-zero where read) */
   struct cnaf_return {
        char X;   /* CAMAC X status */
        char Q;   /* CAMAC Q status */
        char Cok; /* Cok=0: no such crate; =1 crate exists */
        char Online; /* =0 crate offline; =1 crate online */
        int Data;
   };

#endif    /*  CNAF_H_   */
