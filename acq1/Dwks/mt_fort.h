void mt_openro_(char *, int *);
void mt_openrw_(char *, int *);
void mt_close_(int *);
void mt_rew_( int *, int *);
void mt_where_( int *, int *);
void mt_resetpos_( int *, int *);
void mt_rewul_( int *, int *);
void mt_cse_( int *, int *);
void mt_fr_( int *, int *, int *);
void mt_br_( int *, int *, int *);
void mt_ff_( int *, int *, int *);
void mt_bf_( int *, int *, int *);
void mt_weof_( int *, int *);
void mt_getstatus_( int *, int *t, int *, int *, int *, int *);
void dev_getstatus_( int *, unsigned *, unsigned *, int *, int *);

#ifdef __ultrix
void mt_readw_(int *, char *, int *, int *, int *);
void mt_writew_(int *, char *, int *, int *, int *);
void mt_read_(int *, char *, int *, int *, int *);
void mt_write_(int *, char *, int *, int *, int *);
void mt_wait_(int *, char *, int *, int *);
void mt_waitnd_(int *, char *, int *, int *, int *);
void mt_nb_off_(int *, int *);
void mt_ioclr_(int *, int *);
void mt_nb_on_(int *, int *, int *);
void mt_nbnd_on_(int *, int *, int *);
void mt_nd_on_(int *, int *);
void mt_holdnb_(int *, int *, int *);
void mt_r_holdnb_(int *, int *, int *);
void mt_w_holdnb_(int *, int *, int *);
#endif

void to_handle(int , int , struct sigcontext *);
void check_rtimer_(void);
void set_rtimer_(int *);
void set_rtimer_s_(int *);
void set_rtimer_m_(int *);
void set_rtimer_h_(int *);
void get_sys_error_(int *, char *);
void get_f77_error_(int *, char *);
void his_openrw_(char *, int *);
void sys_create_(char *, int *);
void his_openro_(char *, int *);
void his_read_(int *, char *, int *, int *, int *, int *);
void his_write_(int *, char *, int *, int *, int *, int *);
void sys_openrw_(char *, int *);
void sys_openro_(char *, int *);
void sys_close_(int *);
void shm_get_hisspace_(int *, int *, int *);
void shm_open_(int *, int *);
void shm_close_(int *, int *);
void mem_read_hw_(char *, int *, int *);
void mem_read_byte_(char *, int *, int *);
void shm_delete_(int *, int *);
void mem_get_hisspace_(int *, int *);
void mem_zero_hisspace_(int *);
void mem_add1_fw_(int *);
void mem_add1_hw_(int *);
int mem_get_value_fw_(int *);
short int mem_get_value_hw_(int *);
void mem_bufi_(int *, int *, int *, int *, int *);
void mem_bufo_(int *, int *, int *, int *, int *);
void setmemory_(void *, int *, int *);
void copymemory_(void *, void *, int *);
