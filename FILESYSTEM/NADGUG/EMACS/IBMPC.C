/*
 * The routines in this file provide support for the IBM-PC and other
 * compatible terminals. It goes directly to the graphics RAM to do
 * screen output. It compiles into nothing if not an IBM-PC driver
 */

#define	termdef	1			/* don't define "term" external */

#include        <stdio.h>
#include	"estruct.h"
#include        "edef.h"

#if     IBMPC

#define NROW    25                      /* Screen size.                 */
#define NCOL    80                      /* Edit if you want to.         */
#define	MARGIN	8			/* size of minimim margin and	*/
#define	SCRSIZ	64			/* scroll size for extended lines */
#define	NPAUSE	200			/* # times thru update to pause */
#define BEL     0x07                    /* BEL character.               */
#define ESC     0x1B                    /* ESC character.               */
#define	SPACE	32			/* space character		*/
#define	SCADD	0xb8000000L		/* address of screen RAM	*/

int *scptr[NROW];			/* pointer to screen lines	*/
int sline[NCOL];			/* screen line image		*/

extern  int     ttopen();               /* Forward references.          */
extern  int     ttgetc();
extern  int     ttputc();
extern  int     ttflush();
extern  int     ttclose();
extern  int     ibmmove();
extern  int     ibmeeol();
extern  int     ibmeeop();
extern  int     ibmbeep();
extern  int     ibmopen();
extern	int	ibmrev();
extern	int	ibmclose();
extern	int	ibmputc();

#if	COLOR
extern	int	ibmfcol();
extern	int	ibmbcol();

int	cfcolor = -1;		/* current forground color */
int	cbcolor = -1;		/* current background color */
int	ctrans[] =		/* ansi to ibm color translation table */
	{0, 4, 2, 6, 1, 5, 3, 7};
#endif

/*
 * Standard terminal interface dispatch table. Most of the fields point into
 * "termio" code.
 */
TERM    term    = {
        NROW-1,
        NCOL,
	MARGIN,
	SCRSIZ,
	NPAUSE,
        ibmopen,
        ibmclose,
        ttgetc,
	ibmputc,
        ttflush,
        ibmmove,
        ibmeeol,
        ibmeeop,
        ibmbeep,
	ibmrev
#if	COLOR
	, ibmfcol,
	ibmbcol
#endif
};

extern union REGS rg;

#if	COLOR
ibmfcol(color)		/* set the current output color */

int color;	/* color to set */

{
	cfcolor = ctrans[color];
}

ibmbcol(color)		/* set the current background color */

int color;	/* color to set */

{
        cbcolor = ctrans[color];
}
#endif

ibmmove(row, col)
{
	rg.h.ah = 2;		/* set cursor position function code */
	rg.h.dl = col;
	rg.h.dh = row;
	rg.h.bh = 0;		/* set screen page number */
	int86(0x10, &rg, &rg);
}

ibmeeol()	/* erase to the end of the line */

{
	int attr;	/* attribute byte mask to place in RAM */
	int *lnptr;	/* pointer to the destination line */
	int i;
	int ccol;	/* current column cursor lives */
	int crow;	/*	   row	*/

	/* find the current cursor position */
	rg.h.ah = 3;		/* read cursor position function code */
	rg.h.bh = 0;		/* current video page */
	int86(0x10, &rg, &rg);
	ccol = rg.h.dl;		/* record current column */
	crow = rg.h.dh;		/* and row */

	/* build the attribute byte and setup the screen pointer */
#if	COLOR
	attr = (((cbcolor & 15) << 4) | (cfcolor & 15)) << 8;
#else
	attr = 0x0700;
#endif
	lnptr = &sline[0];
	for (i=0; i < NCOL; i++)
		*lnptr++ = SPACE | attr;

	/* wait for vertical retrace to be off */
	while ((inp(0x3da) & 8))
		;

	/* and to be back on */
	while ((inp(0x3da) & 8) == 0)
		;

	/* and send the string out */
	movmem(&sline[0], scptr[crow]+ccol, (NCOL-ccol)*2);

}

ibmputc(ch)	/* put a character at the current position in the
		   current colors */

int ch;

{
	rg.h.ah = 14;		/* write char to screen with current attrs */
	rg.h.al = ch;
#if	COLOR
	rg.h.bl = cfcolor;
#else
	rg.h.bl = 0x07;
#endif
	int86(0x10, &rg, &rg);
}

ibmeeop()
{
	int attr;		/* attribute to fill screen with */

	rg.h.ah = 6;		/* scroll page up function code */
	rg.h.al = 0;		/* # lines to scroll (clear it) */
	rg.x.cx = 0;		/* upper left corner of scroll */
	rg.x.dx = 0x174f;	/* lower right corner of scroll */
#if	COLOR
	attr = ((ctrans[gbcolor] & 15) << 4) | (ctrans[gfcolor] & 15);
#else
	attr = 0;
#endif
	rg.h.bh = attr;
	int86(0x10, &rg, &rg);
}

ibmrev(state)		/* change reverse video state */

int state;	/* TRUE = reverse, FALSE = normal */

{
	/* This never gets used under the IBM-PC driver */
}

ibmbeep()
{
	bdos(6, BEL, 0);
}

ibmopen()
{
	scinit();
	revexist = TRUE;
        ttopen();
}

ibmclose()

{
#if	COLOR
	ibmfcol(7);
	ibmbcol(0);
#endif
	ttclose();
}

scinit()	/* initialize the screen head pointers */

{
	union {
		long laddr;	/* long form of address */
		int *paddr;	/* pointer form of address */
	} addr;
	int i;

	/* initialize the screen pointer array */
	for (i = 0; i < NROW; i++) {
		addr.laddr = SCADD + (long)(NCOL * i * 2);
		scptr[i] = addr.paddr;
	}
}

scwrite(row, outstr, forg, bacg)	/* write a line out*/

int row;	/* row of screen to place outstr on */
char *outstr;	/* string to write out (must be NCOL long) */
int forg;	/* forground color of string to write */
int bacg;	/* background color */

{
	int attr;	/* attribute byte mask to place in RAM */
	int *lnptr;	/* pointer to the destination line */
	int i;

	/* build the attribute byte and setup the screen pointer */
#if	COLOR
	attr = (((ctrans[bacg] & 15) << 4) | (ctrans[forg] & 15)) << 8;
#else
	attr = (((bacg & 15) << 4) | (forg & 15)) << 8;
#endif
	lnptr = &sline[0];
	for (i=0; i<NCOL; i++)
		*lnptr++ = (outstr[i] & 255) | attr;

	/* wait for vertical retrace to be off */
	while ((inp(0x3da) & 8))
		;

	/* and to be back on */
	while ((inp(0x3da) & 8) == 0)
		;

	/* and send the string out */
	movmem(&sline[0], scptr[row],NCOL*2);
}
#else
ibmhello()
{
}
#endif
