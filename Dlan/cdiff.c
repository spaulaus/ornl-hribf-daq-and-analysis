#include <stdio.h>
#define MAXC 257

char *iptrc, *optr;
char in_line[MAXC];
char out_line[MAXC];

FILE *infile, *outfile;

main(int argc, char *argv[])
{
   int count = 0;

/*
*   Open the input file
*/
   if ((infile = fopen("xxx.xxx","r")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open input file\n");
       exit(2);
     }
/*
*   Open the output file
*/
   if ((outfile = fopen("yyy.yyy","w")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open output file - yyy.yyy\n");
       exit(3);
     }
   optr = argv[1];
while (fgets(in_line,MAXC,infile) != NULL)
 {
   iptrc = in_line;
   while(*iptrc != '\n')
     {
       iptrc++;
     }
   *iptrc = '\0';
   strcpy(out_line,"echo \'START ******************* ");
   strcat(out_line,in_line);
   strcat(out_line," *************************\'");
   strcat(out_line,"\n");
   fwrite(out_line, sizeof(char), strlen(out_line), outfile);
   strcpy(out_line,"diff ");
   strcat(out_line,"/tera/mcsq/Dlinux/Dlan/");
   strcat(out_line,in_line);
   strcat(out_line,"  /tera/mcsq/Dlan/");
   strcat(out_line,in_line);
   strcat(out_line,"\n");
   fwrite(out_line, sizeof(char), strlen(out_line), outfile);
   strcpy(out_line,"echo \'END ********************* ");
   strcat(out_line,in_line);
   strcat(out_line," *************************\'");
   strcat(out_line,"\n");
   fwrite(out_line, sizeof(char), strlen(out_line), outfile);
 }
fclose(outfile);
fclose(infile);
exit(0);
}
