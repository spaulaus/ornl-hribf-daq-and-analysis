#ifndef  FASTBUS_H_
#define  FASTBUS_H_
/*  fastbus.h    */
/*  Include definitions widely useful to the fastbus server and client */

#define  MAX_FAST  364

struct fastd {
           char  A;
           char  F;
       short int Data;
} ;
struct fast_buffer {
       short int count;
       struct fastd fasts[MAX_FAST];
} ;

   struct fast_return {
        short int Status;
        short int Data;
   } ;
#endif  /* FASTBUS_H_ */
