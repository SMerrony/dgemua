/*
 *  DG terminal handling routines
 *
 *  Known types are:
 *     $TTY - raw teletype - also assumed to be a VT100 compatible
 *     $CRT1 - Tek 4010I
 *     $CRT2 - DG 6012
 *     $CRT3 - DG 605X
 *     $CRT4 - Unknown
 *     $CRT5 - Unknown - Pseudo DG 6012
 *     $CRT6 - DG 6130
 *     $CRT7 - TVI912 (I wanted it that way)
 *    written by John Toebes and Rodney Radford
 *    with a lot of snide comments by Gordon Keener
 */


#include        <stdio.h>
#include        "estruct.h"
#include	"edef.h"


#if     DGVT

#define	termdef	1			/* don't define "term" external */


#include <paru.h>
#include <sys_calls.h>

extern  int     ttopen();               /* Forward references.          */
extern  int     ttgetc();
extern  int     ttputc();
extern  int     ttflush();
extern  int     ttclose();
extern  int	dgopen();
extern  int     dgclose();
extern  int	dgeeol();
extern  int	dgeeop();
extern  int	dgbeep();
extern  int	dgmove();
extern	int	dgrev();
extern  int	eolexist;
#if	COLOR
extern	int	dgfcol();
extern	int	dgbcol();
#endif

#define	NROWS	24			/* # of screen rolls		*/
#define	NCOLS	80			/* # of screen columns		*/
#define	MARGIN	8			/* size of minimim margin and	*/
#define	SCRSIZ	64			/* scroll size for extended lines */
#define	NPAUSE	100			/* # times thru update to pause */
#define ESC     0x1B
/*
 * Dispatch table. All the
 * hard fields just point into the
 * terminal I/O code.
 */
TERM    term    = {
	NROWS - 1,
	NCOLS,
	MARGIN,
	SCRSIZ,
	NPAUSE,
        &dgopen,
        &dgclose,
        &ttgetc,
        &ttputc,
        &ttflush,
        &dgmove,
        &dgeeol,
        &dgeeop,
        &dgbeep,
        &dgrev
#if	COLOR
	, &dgfcol,
	&dgbcol
#endif
};

char *termrev;			/* reverse video on for status line	*/
char *termnrev;			/* reverse video off "    "     "	*/
char *termeop;			/* Erase to end of page string		*/
int eoppad;			/* Number of pad characters after eop	*/
char *termeol;			/* Erase to end of line string		*/
int eolpad;			/* Number of pad characters after eol	*/
int  termtype;			/* Terminal type identifier		*/


/*******
 *  ttputs - Send a string to ttputc
 *******/

ttputs(string)
char * string;
{
	while (*string != '\0')
		ttputc(*string++);
}


/*******
 *  dgpad - Pad the output after an escape sequence
 *******/

dgpad(count)
int count;
{
	while (count-- > 0)
		ttputc('\0');
}


/*******
 *  dgmove - Move the cursor
 *******/

dgmove(row, col)
{
	switch (termtype) {
		case $TTY:
			ttputc(033);
			ttputc('[');
			ansiparm(row+1);
			ttputc(';');
			ansiparm(col+1);
			ttputc('H');
			break;
		case $CRT3:
			ttputc(020);
			ttputc(col);
			ttputc(row);
			break;
	}
}

/*******
 *  ansiparm (copied from ansi.c) - generate decimal number for ansi terminals
 *******/

ansiparm(n)
register int    n;
{
        register int q,r;

        q = n/10;
        if (q != 0) {
		r = q/10;
		if (r != 0) {
			ttputc((r%10)+'0');
		}
		ttputc((q%10) + '0');
        }
        ttputc((n%10) + '0');
}

/*******
 *  dgrev - set the reverse video status
 *******/

dgrev(status)

int status;	/* TRUE = reverse video, FALSE = normal video */
{
#if	COLOR
		int ftmp, btmp;		/* temporaries for colors */
#endif

	switch (termtype) {
		case $TTY:
			ttputc(ESC);
			ttputc('[');
			ttputc(status ? '7': '0');
			ttputc('m');
#if	COLOR
			if (status == FALSE) {
				ftmp = cfcolor;
				btmp = cbcolor;
				cfcolor = -1;
				cbcolor = -1;
				ansifcol(ftmp);
				ansibcol(btmp);
			}
#endif
			break;
		case $CRT3:
			if (status) {
                           ttputc('\036');
			   ttputc('\104');
			   }
 		        else
			   {
			   ttputc('\036');
			   ttputc('\105');
			   }
			break;
	}
}

#if	COLOR
/*******
 *  dgfcol - Set the forground color (not implimented)
 *******/
 
dgfcol(color)
int color;	/* color to set */
{
if (color == cfcolor)
	return;
cfcolor = color;

switch(termtype)
	{
	case $TTY:
		ttputc(ESC);
		ttputc('[');
		ansiparm(color+30);
		ttputc('m');
		break;
	}
}

/*******
 *  dgbcol - Set the background color (not implimented)
 *******/
 
dgbcol(color)
int color;	/* color to set */
{
if (color == cbcolor)
	return;
cbcolor = color;

switch(termtype)
	{
	case $TTY:
		ttputc(ESC);
		ttputc('[');
		ansiparm(color+40);
		ttputc('m');
		break;
	}
}
#endif

/*******
 *  dgeeol - Erase to end of line
 *******/

dgeeol()
{
	ttputs(termeol);
	dgpad(eolpad);
}


/*******
 *  dgeeop - Erase to end of page (clear screen)
 *******/

dgeeop()
{

	ttputs(termeop);
	dgpad(eoppad);
}


/*******
 *  dgbeep - Ring the bell
 *******/

dgbeep()
{
	ttputc('\007');
}

/*******
 *  dgopen - Get terminal type and open terminal
 *******/

dgopen()
{
	termtype = dggtty();
	switch (termtype) {
		case $TTY:  /* assume TEK 4105 */
			termeol = "\033[K";
			termeop = "\033[J";
 			eoppad = 0;
			/* now reset the scrolling region size */
			ttputc(ESC);
			ttputc('[');
			ttputc('1');
			ttputc(';');
			ansiparm(term.t_nrow);
			ttputc('r');
			/* if they requested it, set them into 132 column mode*/
			if (term.t_ncol > 80)
				ttputs("");
			break;
		case $CRT3:   	/* D200 */
			termeol = "\013";
			termeop = "\014";
			eoppad = 0;
			break;
		default:
	 	        printf("Terminal type %d not supported\n",
					termtype);
			exit (1);
	}
        ttopen();
}


/*******
 *  dgclose - resets terminal state, close's terminal
 *******/
dgclose()
{
     if (termtype == $TTY) {   /* VT100, TEK4105 */
        ttputc(ESC);	       /* disable page mode */
	ttputc('[');
	ttputc('r');
        dgmove(term.t_nrow, term.t_ncol);
        }

     ttclose();
}

 
   
/*******
 *  dggtty - Get terminal type from system control block
 *******/

dggtty()
{
short flags[3];
int fchannel();

if (sys_gchr(fchannel(stdin),1<<31,&flags))
   {
   perror("?GCHR");
   exit(0);
   }

term.t_nrow = (flags[2] >> 8) - 1;
term.t_ncol = flags[2] & 255;
return(flags[1] & $DTYPE);
}

#else

hellodg()

{
}

#endif	DGVT

