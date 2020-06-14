/*
*   Function Prototype
*/
static int chtype(char);

#define    ALPHA      1
#define    NUMERIC    2
#define    DELIM      3
#define    MAXFIELDS  10
/******************************************************************************
*
*  This routine is very similar in function to Milner's gread.  The major
*  difference is that 1) the input string is a character*(*) variable and
*  2) indices of the start and end of fields are returned.  Since the
*  fields are not copied to new storage, fields are limited only by the
*  maximum length of the call character variable.
*
*            character*xxx  line
*            integer*4  indx(2,10),itype(10),nf,inext
*
*  CALL:  
*         call strparse(line(imin:imax),indx,itype,nf,inext)
*  CALL PARAMETERS:
*            line  - character*(*) variable.
*  RETURN:
*            nf    - number of fields found in line
*            inext - If nonzero, the number of fields exceeds MAXFIELDS.
*                    If that is the case, redo is the character index
*                    where you should continute the parse.
*            indx  - array of indices for the start and end of a field
*            ityp  - array of field types. 1 means alpha first char and
*                    2 means numeric first char
*
******************************************************************************/
void strparse_(char *line,int *indx,int *itype,int *nf,int *inext,int lenline)
{
    int  im,i,j,l,m;

    *nf = 0;
    *inext = 0;
    if (lenline <= 0) return;
    j = 0;
/*
*     Find the length of the string stored in line.  If the length is
*     less than the start index, return with nf = 0.
*/
    im = strlen_(line,lenline);
    if (im <= 0) return;
/*
*     Find the first char of the field.  If no more fields, just return.
*/
    while(1)
      {
        for(i=j; i < im; i++) if (chtype(line[i]) != DELIM) goto Field;
        return;
/*
*   See if we have room for another field( only MAXFIELDS fields allowed).
*   If not, return number of characters we did not process.
*/
Field:
        m = *nf;
        if (m >= MAXFIELDS)
          {
            *inext = i + 1;
            return;
          }
        l = m * 2;
        *nf = m + 1;
/*
*  Save index of the first char of the field and determine the field
*  type (i.e. numeric or alpha)
*/
        indx[l] = i + 1;
        itype[m] = chtype(line[i]);
/*
*  Find end of field and save the index
*/
        for (j = i; j < im; j++) if (chtype(line[j]) == DELIM) goto EndField;
        indx[l + 1] = j;
        return;
 
EndField:
        indx[l + 1] = j;
   }
}
/******************************************************************************
*
*   Function to determine character "type"
*
*   chtype = 1 means Non-numeric but not delimiter
*   chtype = 2 means Numeric (Digits 0 thru 9 + - . )
*   chtype = 3 means A delimiter (Blank , = tab )
*
******************************************************************************/
static int chtype(char c)
{
   if (c >= '0' && c <= '9') return NUMERIC;
   if (c == '+' || c == '-' || c == '.') return NUMERIC;
   if (c == ' ' || c == ',' || c == '=') return DELIM;
   if (c == '\t' || c == '\0' || c == '\n') return DELIM;

   return ALPHA;
}
