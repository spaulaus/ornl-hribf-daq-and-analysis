/*==================================================================
  en_close.c closes the currently open packet filter device
  ===================================================================
*/

void en_close(int pfd) 
{
   close(pfd);
}
