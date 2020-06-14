void main()
{
   void (*bios)(void) = (void (*)())0x17000;

   bios();
}
