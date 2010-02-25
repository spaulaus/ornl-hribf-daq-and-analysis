#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

#include "sis3820.h"

main()
  {
    unsigned long tmp;

    printf("his_last_cnt = 0x%x\n",offsetof(struct SIS3820,his_last_cnt));
    printf("op_mode   = 0x%x\n",offsetof(struct SIS3820,op_mode  ));
    printf("preset_ch_sel  = 0x%x\n",offsetof(struct SIS3820,preset_ch_sel ));
    printf("cnt_inhibit    = 0x%x\n",offsetof(struct SIS3820,cnt_inhibit   ));
    printf("tst_pulse_msk   = 0x%x\n",offsetof(struct SIS3820,tst_pulse_msk  ));
    printf("Key_reset = 0x%x\n",offsetof(struct SIS3820,Key_reset));
    printf("Key_disable = 0x%x\n",offsetof(struct SIS3820,Key_disable));
    printf("shadow[0] = 0x%x\n",offsetof(struct SIS3820,shadow[0]));
    printf("counter[0] = 0x%x\n",offsetof(struct SIS3820,counter[0]));
  }
