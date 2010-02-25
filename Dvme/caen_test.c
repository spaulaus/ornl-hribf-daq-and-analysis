#include "vme_sys.h"
#include "vmeprom.h"
#include "caen.h"


void main()
{
    int  i;
    struct CAEN *caen = (struct CAEN *)CAEN_ADC1;

    caen->bit_set2 = 0x01;
    caen->mem_test_addr = 0;
    caen->test_addr = 0x100;
    caen->mem_test_word_h = 0x55aa;
    caen->mem_test_word_l = 0x1234;
    caen->mem_test_addr = 0x110;
    caen->test_addr = 0;
/*     i = caen->buf[0]; */
    caen->bit_clear2 = 0x01;
    printf("data = %x\n",i);
}

