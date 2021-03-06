/* JRB 6/90     TIMER STUFF
--------------------------------
SOURCE FILE:  /usr/users/beene/Djblibs/timer_stuff.c   
LIBRARY:      /usr/users/beene/lib/jblibc1.a
---------------------------------------------------------------
  This is a closely coupled set of routines designed to aid
  in smooothing exabyte reads. Two of the routines 
  SET_RTIMER(ITIME)          ITIME= countdown time in millisecs.
  CHECK_RTIMER()
  are fortran callable. The third
  to_handler(..)
  is a signal (interrupt) handler and shouldn't be called by you.

  POSIX compliant calls are used for manipulation of signal masks.
  
  SET_RTIMER(ITIME) sets up an alarm to go off ITIME ms later. It
  uses wall clock time (ITIMER_REAL). It all so sets up a handler 
  (to_handler) to handle the signal SIGALRM generated when the 
  timer goes off. The handler merely sets a flag to say the timer
  has expired, disables the timer and returns.
  
  CHECK_RTIMER() checks the status of the timer. Ifthe timer has
  expired it returns immediately. If not it goes to sleep and 
  waits for the signal, then returns.
*/


#include <sys/time.h>
#include <signal.h>
int      to_flag=0;
int      calls = 0;
sigset_t oset, eset, sigsat;
struct itimerval nval, oval;

//void to_handle(int sig, int code, struct sigcontext *scp){
void to_handle(int sig, int code){
    to_flag=1;
    nval.it_interval.tv_sec =0;
    nval.it_interval.tv_usec =0; 
    nval.it_value.tv_sec =0; 
    nval.it_value.tv_usec =0;
    setitimer(ITIMER_REAL,&nval,&oval);    /*  Disable timer */
}
void stop_rtimer__(){
    int st;
    if(calls == 0)                         /* no timer set up! */
       return;    
    st = sigprocmask(SIG_BLOCK, &sigsat, &oset);  /* block SIGALRM */
/*     Stop the timer    */
    nval.it_interval.tv_sec =0;
    nval.it_interval.tv_usec =0; 
    nval.it_value.tv_sec =0; 
    nval.it_value.tv_usec =0;
    setitimer(ITIMER_REAL,&nval,&oval);    /*  Disable timer */
    to_flag=0;
}   
void check_rtimer__(){
    int st;
    if(calls == 0)                         /* no timer set up! */
       return;    
    st = sigprocmask(SIG_BLOCK, &sigsat, &oset);  /* block SIGALRM */
    if(to_flag == 0){
       sigsuspend(&eset);                   /* Wait for any signal */
       to_flag=0;
    }
    else
       to_flag=0;
    st = sigprocmask(SIG_UNBLOCK, &sigsat, &oset);  /* unblock SIGALRM */
}   
void set_rtimer__(int *atime){          /* *atime is time for timer in ms */
    int time,times,timeu,st;
    if(calls == 0){
        st = sigaddset(&sigsat,SIGALRM);  /* do some setup stuff*/
        st = sigemptyset(&eset);
        calls = 1;
    }
    time = *atime;
    times= time/1000;
    timeu = (time % 1000) * 1000;
    nval.it_interval.tv_sec =0;
    nval.it_interval.tv_usec =0; 
    nval.it_value.tv_sec =times;        /*  Timer time sec. */
    nval.it_value.tv_usec =timeu;       /*  Timer time micro sec. */
    to_flag=0;
    st = sigprocmask(SIG_UNBLOCK, &sigsat, &oset);   /* Make SIGALRM unblocked */
    signal(SIGALRM,(void (*))to_handle);/*  Set up handler for SIGLARM */
    setitimer(ITIMER_REAL,&nval,&oval); /*  Start timer */
}
void set_rtimer_s__(int *atime){         /* *atime is time for timer in sec */
    int st;
    if(calls == 0){
        st = sigaddset(&sigsat,SIGALRM);  /* do some setup stuff*/
        st = sigemptyset(&eset);
        calls = 1;
    }
    nval.it_interval.tv_sec =0;
    nval.it_interval.tv_usec =0; 
    nval.it_value.tv_sec = *atime;      /*  Timer time sec. */
    nval.it_value.tv_usec =0;           /*  Timer time micro sec. */
    to_flag=0;
    st = sigprocmask(SIG_UNBLOCK, &sigsat, &oset);   /*Make SIGALRM unblocked*/
    signal(SIGALRM,(void (*))to_handle);/*  Set up handler for SIGLARM */
    setitimer(ITIMER_REAL,&nval,&oval); /*  Start timer */
}
void set_rtimer_m__(int *atime){         /* *atime is time for timer in min */
    int st;
    if(calls == 0){
        st = sigaddset(&sigsat,SIGALRM);  /* do some setup stuff*/
        st = sigemptyset(&eset);
        calls = 1;
    }
    nval.it_interval.tv_sec =0;
    nval.it_interval.tv_usec =0; 
    nval.it_value.tv_sec = (*atime) * 60; /*  Timer time sec. */
    nval.it_value.tv_usec =0;           /*  Timer time micro sec. */
    to_flag=0;
    st = sigprocmask(SIG_UNBLOCK, &sigsat, &oset);   /*Make SIGALRM unblocked*/
    signal(SIGALRM,(void (*))to_handle);/*  Set up handler for SIGLARM */
    setitimer(ITIMER_REAL,&nval,&oval); /*  Start timer */
}
void set_rtimer_h__(int *atime){         /* *atime is time for timer in hrs */
    int st;
    if(calls == 0){
        st = sigaddset(&sigsat,SIGALRM);  /* do some setup stuff*/
        st = sigemptyset(&eset);
        calls = 1;
    }
    nval.it_interval.tv_sec =0;
    nval.it_interval.tv_usec =0; 
    nval.it_value.tv_sec = (*atime) * 3600;   /*  Timer time sec. */
    nval.it_value.tv_usec =0;           /*  Timer time micro sec. */
    to_flag=0;
    st = sigprocmask(SIG_UNBLOCK, &sigsat, &oset);   /*Make SIGALRM unblocked*/
    signal(SIGALRM,(void (*))to_handle);/*  Set up handler for SIGLARM */
    setitimer(ITIMER_REAL,&nval,&oval); /*  Start timer */
}
      

