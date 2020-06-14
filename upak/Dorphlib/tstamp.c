#include <time.h>

void timestamp_(char *string)
{
   time_t tod;

   time(&tod);
   strftime(string,21,"%d-%b-%y %H:%M:%S",localtime(&tod));
}
