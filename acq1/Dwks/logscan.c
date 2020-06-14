#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>


void logexit(void);

/*   Global variables       */

#define MAXC 257
char in_line[MAXC],lfile[81];
char *extract[] = {"VMEacq  ","FILEOU  ","pacman  ","pftoipc ","scad    ",
                   "SMI     ","SETDISCS","HVMASTER","vmemon  ",NULL};
char     *key[] = {"vmeacq","fileou","pacman","pftoipc","scad",
                   "smi","setdiscs","hvmaster","vmemon",NULL};
FILE *infile;

/****************************************************************************
****************************************************************************/
main(int argc, char *argv[])
{
  int  i,key_index;
  int left_end,mv_count,right_end;
  char *cptr,*cptr2;

  if (argc < 2) logexit();
  cptr = argv[1];
  if (*cptr == '/')
    {
      if (argc < 3) logexit();
      strcpy(lfile, cptr);
      cptr = argv[2];
    }
  else
    {
      strcpy(lfile, getenv("HOME"));
    }
  strcat(lfile,"/orphas.");
  if ((cptr2 = getenv("VME")) != NULL) strcat(lfile,cptr2);
  else  strcat(lfile,"vme");
  cptr2 = cptr;
  for (i=0; i < strlen(argv[1]); ++i) {*cptr = tolower(*cptr); ++cptr2;}
  for (i=0; key[i] != NULL; i++)
   {
     if (!strcmp(cptr,key[i])) break;
   }
  if (key[i] == NULL) logexit();
  key_index = i;
/*
*   Open the input file
*/
   if ((infile = fopen(lfile,"r")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open input file - %s\n",lfile);
       exit(2);
     }
   printf("Using log file - %s\n",lfile);
   while (fgets(in_line,MAXC,infile) != NULL)
     {
       int len;

       if (!strncmp(in_line,extract[key_index],8))
         {
           len = strlen(in_line);
           in_line[--len] = '\0';
           if (len > 80)
             {
/*
*  Most log file entries have text at the start of the line and text near the
*  end of the line.  See if there are spaces which can be replaced with
*  text at the end of the line such that the line will fit 80 columns.
*/
               mv_count = 0;
               left_end = 0;
               right_end = len;
/*
*  Find number of characters which would have to be moved and the starting
*  position on the line.
*/
               for (i=len-1; i > 79; --i)
                 {
                   if (in_line[i] != ' ') right_end = i;
                  }
                mv_count = len - right_end;
/*
*  Find the end of text starting from the left.
*/
                for (i=29; i < 80; ++i)
                  {
                    if (in_line[i] != ' ') left_end = i +1;
                  }
/*
*  See if we can fit it on one 80 column line
*/
                if(left_end + mv_count > 80)
                 {
/*
*  No can do - output as two lines
*/
                   for (i=79; i > 29; --i)
                    {
                      if (in_line[i] == ' ')
                        {
                          in_line[i] = '\0';
                          break;
                        }
                    }
                   printf("%s\n",in_line);
                   printf("%80s\n",&in_line[i+1]);
                 }
                else
                 {
/*
*   Will fit on one line
*/
                   strcpy(&in_line[80-mv_count],&in_line[right_end]);
                   printf("%s\n",in_line);
                 }
             }
           else
/*
*  Original line was 80 characters or less
*/
             {
               printf("%s\n",in_line);
             }
         }
     }
   return(0);
}
/*****************************************************************************
*****************************************************************************/
void logexit(void)
{
   printf("Usage:  logscan [path]  keyword\n");
   printf("Keywords: hvmaster, pacman, pftoipc, scad, setdiscs\n");
   printf("          smi, tapeou, vmeacq, vmemon.\n");
   exit(1);
}
