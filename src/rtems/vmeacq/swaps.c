/****************************************************************************
****************************************************************************/
 unsigned int word_swap(unsigned short *buf,int count)
{
    register unsigned short tmp1;

    while(count > 0)
     {
       tmp1 = *buf;
       *buf = *(buf+1);
       buf++;
       *buf++ = tmp1;
       count -= 2;
     }
    return (*((int *)(buf - 2)));
}
/****************************************************************************
****************************************************************************/
 void byte_swap(unsigned char *buf,int count)
{
    register unsigned char tmp1;

    while(count > 0)
     {
       tmp1 = *buf;
       *buf = *(buf+1);
       buf++;
       *buf++ = tmp1;
       count -= 2;
     }
    return;
}

