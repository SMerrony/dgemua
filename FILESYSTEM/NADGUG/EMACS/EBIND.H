/*	EBIND:		Initial default key to function bindings for
			MicroEMACS 3.7
*/

/*
 * Command table.
 * This table  is *roughly* in ASCII order, left to right across the
 * characters of the command. This expains the funny location of the
 * control-X commands.
 */
KEYTAB  keytab[NBINDS] = {
	{CTRL|'A',		gotobol},
	{CTRL|'B',		backchar},
	{CTRL|'C',		insspace},
	{CTRL|'D',		forwdel},
	{CTRL|'E',		gotoeol},
	{CTRL|'F',		forwchar},
	{CTRL|'G',		ctrlg},
	{CTRL|'H',		backdel},
	{CTRL|'I',		tab},
	{CTRL|'J',		indent},
	{CTRL|'K',		killtext},
	{CTRL|'L',		refresh},
	{CTRL|'M',		newline},
	{CTRL|'N',		forwline},
	{CTRL|'O',		openline},
	{CTRL|'P',		backline},
	{CTRL|'Q',		quote},
	{CTRL|'R',		backsearch},
	{CTRL|'S',		forwsearch},
	{CTRL|'T',		twiddle},
	{CTRL|'V',		forwpage},
	{CTRL|'W',		killregion},
	{CTRL|'X',		cex},
	{CTRL|'Y',		yank},
	{CTRL|'Z',		backpage},
	{CTRL|']',		meta},
	{CTLX|CTRL|'B',		listbuffers},
	{CTLX|CTRL|'C',		quit},          /* Hard quit.           */
	{CTLX|CTRL|'F',		filefind},
	{CTLX|CTRL|'I',		insfile},
	{CTLX|CTRL|'L',		lowerregion},
	{CTLX|CTRL|'M',		delmode},
	{CTLX|CTRL|'N',		mvdnwind},
	{CTLX|CTRL|'O',		deblank},
	{CTLX|CTRL|'P',		mvupwind},
	{CTLX|CTRL|'R',		fileread},
	{CTLX|CTRL|'S',		filesave},
	{CTLX|CTRL|'U',		upperregion},
	{CTLX|CTRL|'V',		viewfile},
	{CTLX|CTRL|'W',		filewrite},
	{CTLX|CTRL|'X',		swapmark},
	{CTLX|CTRL|'Z',		shrinkwind},
	{CTLX|'?',		deskey},
	{CTLX|'!',		spawn},
	{CTLX|'@',		pipe},
	{CTLX|'#',		filter},
	{CTLX|'=',		showcpos},
	{CTLX|'(',		ctlxlp},
	{CTLX|')',		ctlxrp},
	{CTLX|'^',		enlargewind},
	{CTLX|'0',		delwind},
	{CTLX|'1',		onlywind},
	{CTLX|'2',		splitwind},
	{CTLX|'B',		usebuffer},
	{CTLX|'C',		spawncli},
#if	BSD
	{CTLX|'D',		bktoshell},
#endif
	{CTLX|'E',		ctlxe},
	{CTLX|'F',		setfillcol},
	{CTLX|'K',		killbuffer},
	{CTLX|'M',		setmode},
	{CTLX|'N',		filename},
	{CTLX|'O',		nextwind},
	{CTLX|'P',		prevwind},
#if	ISRCH
	{CTLX|'R',		risearch},
	{CTLX|'S',		fisearch},
#endif
	{CTLX|'W',		resize},
	{CTLX|'X',		nextbuffer},
	{CTLX|'Z',		enlargewind},
#if	WORDPRO
	{META|CTRL|'C',		wordcount},
#endif
	{META|CTRL|'H',		delbword},
	{META|CTRL|'K',		unbindkey},
	{META|CTRL|'L',		reposition},
	{META|CTRL|'M',		delgmode},
	{META|CTRL|'N',		namebuffer},
	{META|CTRL|'R',		qreplace},
	{META|CTRL|'V',		scrnextdw},
#if	WORDPRO
	{META|CTRL|'W',		killpara},
#endif
	{META|CTRL|'Z',		scrnextup},
	{META|' ',		setmark},
	{META|'?',		help},
	{META|'!',		reposition},
	{META|'.',		setmark},
	{META|'>',		gotoeob},
	{META|'<',		gotobob},
	{META|'~',		unmark},
	{META|'B',		backword},
	{META|'C',		capword},
	{META|'D',		delfword},
	{META|'F',		forwword},
	{META|'G',		gotoline},
	{META|'K',		bindtokey},
	{META|'L',		lowerword},
	{META|'M',		setgmode},
#if	WORDPRO
	{META|'N',		gotoeop},
	{META|'P',		gotobop},
	{META|'Q',		fillpara},
#endif
	{META|'R',		sreplace},
#if	BSD
	{META|'S',		bktoshell},
#endif
	{META|'U',		upperword},
	{META|'V',		backpage},
	{META|'W',		copyregion},
	{META|'X',		namedcmd},
	{META|'Z',		quickexit},
	{META|0x7F,              delbword},

#if	MSDOS & (HP150 == 0) & (WANGPC == 0)
	{SPEC|CTRL|'_',		forwhunt},
	{SPEC|CTRL|'S',		backhunt},
	{SPEC|71,		gotobob},
	{SPEC|72,		backline},
	{SPEC|73,		backpage},
	{SPEC|75,		backchar},
	{SPEC|77,		forwchar},
	{SPEC|79,		gotoeob},
	{SPEC|80,		forwline},
	{SPEC|81,		forwpage},
	{SPEC|82,		insspace},
	{SPEC|83,		forwdel},
	{SPEC|115,		backword},
	{SPEC|116,		forwword},
	{SPEC|132,		gotobop},
	{SPEC|118,		gotoeop},
	{SPEC|84,		cbuf1},
	{SPEC|85,		cbuf2},
	{SPEC|86,		cbuf3},
	{SPEC|87,		cbuf4},
	{SPEC|88,		cbuf5},
	{SPEC|89,		cbuf6},
	{SPEC|90,		cbuf7},
	{SPEC|91,		cbuf8},
	{SPEC|92,		cbuf9},
	{SPEC|93,		cbuf10},
#endif

#if	HP150
	{SPEC|32,		backline},
	{SPEC|33,		forwline},
	{SPEC|35,		backchar},
	{SPEC|34,		forwchar},
	{SPEC|44,		gotobob},
	{SPEC|46,		forwpage},
	{SPEC|47,		backpage},
	{SPEC|82,		nextwind},
	{SPEC|68,		openline},
	{SPEC|69,		killtext},
	{SPEC|65,		forwdel},
	{SPEC|64,		ctlxe},
	{SPEC|67,		refresh},
	{SPEC|66,		reposition},
	{SPEC|83,		help},
	{SPEC|81,		deskey},
#endif

#if	AMIGA
	{SPEC|'?',		help},
	{SPEC|'A',		backline},
	{SPEC|'B',		forwline},
	{SPEC|'C',		forwchar},
	{SPEC|'D',		backchar},
	{SPEC|'T',		backpage},
	{SPEC|'S',		forwpage},
	{SPEC|'a',		backword},
	{SPEC|'`',		forwword},
	{SPEC|'P',		cbuf1},
	{SPEC|'Q',		cbuf2},
	{SPEC|'R',		cbuf3},
	{SPEC|'S',		cbuf4},
	{SPEC|'T',		cbuf5},
	{SPEC|'U',		cbuf6},
	{SPEC|'V',		cbuf7},
	{SPEC|'W',		cbuf8},
	{SPEC|'X',		cbuf9},
	{SPEC|'Y',		cbuf10},

#endif

#if  WANGPC
	{SPEC|0xE0,              quit},           /* Cancel */
	{SPEC|0xE1,              help},           /* Help */
	{SPEC|0xF1,              help},           /* ^Help */
	{SPEC|0xE3,              ctrlg},          /* Print */
	{SPEC|0xF3,              ctrlg},          /* ^Print */
	{SPEC|0xC0,              backline},       /* North */
	{SPEC|0xD0,              gotobob},        /* ^North */
	{SPEC|0xC1,              forwchar},       /* East */
	{SPEC|0xD1,              gotoeol},        /* ^East */
	{SPEC|0xC2,              forwline},       /* South */
	{SPEC|0xD2,              gotobop},        /* ^South */
	{SPEC|0xC3,              backchar},       /* West */
	{SPEC|0xD3,              gotobol},        /* ^West */
	{SPEC|0xC4,              ctrlg},          /* Home */
	{SPEC|0xD4,              gotobob},        /* ^Home */
	{SPEC|0xC5,              filesave},       /* Execute */
	{SPEC|0xD5,              ctrlg},          /* ^Execute */
	{SPEC|0xC6,              insfile},        /* Insert */
	{SPEC|0xD6,              ctrlg},          /* ^Insert */
	{SPEC|0xC7,              forwdel},        /* Delete */
	{SPEC|0xD7,              killregion},     /* ^Delete */
	{SPEC|0xC8,              backpage},       /* Previous */
	{SPEC|0xD8,              prevwind},       /* ^Previous */
	SPEC|0xC9,              forwpage,       /* Next */
	SPEC|0xD9,              nextwind,       /* ^Next */
	SPEC|0xCB,              ctrlg,          /* Erase */
	SPEC|0xDB,              ctrlg,          /* ^Erase */
	SPEC|0xDC,              ctrlg,          /* ^Tab */
	SPEC|0xCD,              ctrlg,          /* BackTab */
	SPEC|0xDD,              ctrlg,          /* ^BackTab */
	SPEC|0x80,              ctrlg,          /* Indent */
	SPEC|0x90,              ctrlg,          /* ^Indent */
	SPEC|0x81,              ctrlg,          /* Page */
	SPEC|0x91,              ctrlg,          /* ^Page */
	SPEC|0x82,              ctrlg,          /* Center */
	SPEC|0x92,              ctrlg,          /* ^Center */
	SPEC|0x83,              ctrlg,          /* DecTab */
	SPEC|0x93,              ctrlg,          /* ^DecTab */
	SPEC|0x84,              ctrlg,          /* Format */
	SPEC|0x94,              ctrlg,          /* ^Format */
	SPEC|0x85,              ctrlg,          /* Merge */
	SPEC|0x95,              ctrlg,          /* ^Merge */
	SPEC|0x86,              setmark,        /* Note */
	SPEC|0x96,              ctrlg,          /* ^Note */
	SPEC|0x87,              ctrlg,          /* Stop */
	SPEC|0x97,              ctrlg,          /* ^Stop */
	SPEC|0x88,              forwsearch,     /* Srch */
	SPEC|0x98,              backsearch,     /* ^Srch */
	SPEC|0x89,              sreplace,       /* Replac */
	SPEC|0x99,              qreplace,       /* ^Replac */
	SPEC|0x8A,              ctrlg,          /* Copy */
	SPEC|0x9A,              ctrlg,          /* ^Copy */
	SPEC|0x8B,              ctrlg,          /* Move */
	SPEC|0x9B,              ctrlg,          /* ^Move */
	SPEC|0x8C,              namedcmd,       /* Command */
	SPEC|0x9C,              spawn,          /* ^Command */
	SPEC|0x8D,              ctrlg,          /* ^ */
	SPEC|0x9D,              ctrlg,          /* ^^ */
	SPEC|0x8E,              ctrlg,          /* Blank */
	SPEC|0x9E,              ctrlg,          /* ^Blank */
	SPEC|0x8F,              gotoline,       /* GoTo */
	SPEC|0x9F,              usebuffer,      /* ^GoTo */
#endif
#if	DG
	{SPEC|0161,		backpage},		/* F1 */
	{SPEC|0162,		forwpage},		/* F2 */
	{SPEC|0163,		swapmark},		/* F3 */
	{SPEC|0164,		onlywind},		/* F4 */
	{SPEC|0165,		splitwind},		/* F5 */
	{SPEC|0166,		forwsearch},		/* F6 */
	{SPEC|0167,		forwsearch},		/* F7 */
	{SPEC|0170,		killbuffer},		/* F8 */
	{SPEC|0171,		cex},		/* F9 */
	{SPEC|0172,		cex},		/* F10 */
	{SPEC|0173,		cex},		/* F11 */
	{SPEC|0174,		cex},		/* F12 */
	{SPEC|0175,		cex},		/* F13 */
	{SPEC|0176,		cex},		/* F14 */
	{SPEC|0160,		cex},		/* F15 */
	{SPEC|0141,		gotobop},		/* Shift F1 */
	{SPEC|0142,		gotoeop},		/* Shift F2 */
	{SPEC|0143,		cex},		/* Shift F3 */
	{SPEC|0144,		cex},		/* Shift F4 */
	{SPEC|0145,		cex},		/* Shift F5 */
	{SPEC|0146,		openline},		/* Shift F6 */
	{SPEC|0147,		backsearch},		/* Shift F7 */
	{SPEC|0150,		cex},		/* Shift F8 */
	{SPEC|0151,		cex},		/* Shift F9 */
	{SPEC|0152,		cex},		/* Shift F10 */
	{SPEC|0153,		cex},		/* Shift F11 */
	{SPEC|0154,		cex},		/* Shift F12 */
	{SPEC|0155,		cex},		/* Shift F13 */
	{SPEC|0156,		cex},		/* Shift F14 */
	{SPEC|0140,		cex},		/* Shift F15 */
	{SPEC|0161,		cex},		/* Control F1 */
	{SPEC|0162,		cex},		/* Control F2 */
	{SPEC|0163,		cex},		/* Control F3 */
	{SPEC|0164,		cex},		/* Control F4 */
	{SPEC|0165,		cex},		/* Control F5 */
	{SPEC|0166,		cex},		/* Control F6 */
	{SPEC|0167,		cex},		/* Control F7 */
	{SPEC|0170,		cex},		/* Control F8 */
	{SPEC|0171,		cex},		/* Control F9 */
	{SPEC|0172,		cex},		/* Control F10 */
	{SPEC|0173,		cex},		/* Control F11 */
	{SPEC|0174,		cex},		/* Control F12 */
	{SPEC|0175,		cex},		/* Control F13 */
	{SPEC|0176,		cex},		/* Control F14 */
	{SPEC|0160,		cex},		/* Control F15 */
	{SPEC|0141,		cex},		/* Control Shift F1 */
	{SPEC|0142,		cex},		/* Control Shift F2 */
	{SPEC|0143,		cex},		/* Control Shift F3 */
	{SPEC|0144,		cex},		/* Control Shift F4 */
	{SPEC|0145,		cex},		/* Control Shift F5 */
	{SPEC|0146,		cex},		/* Control Shift F6 */
	{SPEC|0147,		cex},		/* Control Shift F7 */
	{SPEC|0150,		cex},		/* Control Shift F8 */
	{SPEC|0151,		cex},		/* Control Shift F9 */
	{SPEC|0152,		cex},		/* Control Shift F10 */
	{SPEC|0153,		cex},		/* Control Shift F11 */
	{SPEC|0154,		cex},		/* Control Shift F12 */
	{SPEC|0155,		cex},		/* Control Shift F13 */
	{SPEC|0156,		cex},		/* Control Shift F14 */
	{SPEC|0140,		cex},		/* Control Shift F15 */
#endif
	{0x7F,			backdel},
	{0,			NULL}
};

#if RAINBOW

#include "rainbow.h"

/*
 * Mapping table from the LK201 function keys to the internal EMACS character.
 */

short lk_map[][2] = {
	Up_Key,                         CTRL+'P',
	Down_Key,                       CTRL+'N',
	Left_Key,                       CTRL+'B',
	Right_Key,                      CTRL+'F',
	Shift+Left_Key,                 META+'B',
	Shift+Right_Key,                META+'F',
	Control+Left_Key,               CTRL+'A',
	Control+Right_Key,              CTRL+'E',
	Prev_Scr_Key,                   META+'V',
	Next_Scr_Key,                   CTRL+'V',
	Shift+Up_Key,                   META+'<',
	Shift+Down_Key,                 META+'>',
	Cancel_Key,                     CTRL+'G',
	Find_Key,                       CTRL+'S',
	Shift+Find_Key,                 CTRL+'R',
	Insert_Key,                     CTRL+'Y',
	Options_Key,                    CTRL+'D',
	Shift+Options_Key,              META+'D',
	Remove_Key,                     CTRL+'W',
	Shift+Remove_Key,               META+'W',
	Select_Key,                     CTRL+'@',
	Shift+Select_Key,               CTLX+CTRL+'X',
	Interrupt_Key,                  CTRL+'U',
	Keypad_PF2,                     META+'L',
	Keypad_PF3,                     META+'C',
	Keypad_PF4,                     META+'U',
	Shift+Keypad_PF2,               CTLX+CTRL+'L',
	Shift+Keypad_PF4,               CTLX+CTRL+'U',
	Keypad_1,                       CTLX+'1',
	Keypad_2,                       CTLX+'2',
	Do_Key,                         CTLX+'E',
	Keypad_4,                       CTLX+CTRL+'B',
	Keypad_5,                       CTLX+'B',
	Keypad_6,                       CTLX+'K',
	Resume_Key,                     META+'!',
	Control+Next_Scr_Key,           CTLX+'N',
	Control+Prev_Scr_Key,           CTLX+'P',
	Control+Up_Key,                 CTLX+CTRL+'P',
	Control+Down_Key,               CTLX+CTRL+'N',
	Help_Key,                       CTLX+'=',
	Shift+Do_Key,                   CTLX+'(',
	Control+Do_Key,                 CTLX+')',
	Keypad_0,                       CTLX+'Z',
	Shift+Keypad_0,                 CTLX+CTRL+'Z',
	Main_Scr_Key,                   CTRL+'C',
	Keypad_Enter,                   CTLX+'!',
	Exit_Key,                       CTLX+CTRL+'C',
	Shift+Exit_Key,                 CTRL+'Z'
};

#define lk_map_size     (sizeof(lk_map)/2)
#endif

