@chapter(INTRODUCTON)
 
     MicroEMACS, henceforth called EMACS, is a full screen editor (not a
word processor) which is available on a variety of computers (both micro
and mini/main frame).  As a general characteristic, EMACS is a function
driven program wherein every keystroke is a "function" call; hence,
there is no notion of input versus edit modes.  Thus, for example, the
keystroke 'a' invokes the function "insert-character 'a'".  All visible
ASCII (8-bit) characters are self-inserting while the non-visible
(control) characters are used as commands. 
 
There is a tutorial file called emacs.tut on the distribution disk. 
It might be useful if you read this documentation after going through
the tutorial.  To do this, type @b<emacs emacs.tut> when you get the DOS prompt.
This will start emacs, instructing it to read in the file emacs.tut
from the default disk into a standard buffer and put you into it.  You
can then follow the instructions in it.  @i<In case of trouble, you
can always abort a command with CTRL-G or abort emacs with CTRL-X CTRL-C.>

     The command structure of EMACS is simple: commands are
single keystroke, double keystroke, or full command-spec. For
efficiency, single keystroke commands are the most used, followed
by 2-keystroke, and so on.
 
    Every command (where it makes sense) in EMACS allows for a numeric
argument in pre-fix notation.  Since decimal digits are also
self-inserting, arguments are passed to the command (including
self-insert commands) through two mechanisms.  First, an arbitrary
numeric argument can be passed by ESC <number> where <number> is a
standard decimal number and ESC is the ESCape character (control-[ or
the ESC on your keyboard).  Thus, the sequence "esc 7 a" results in
aaaaaaaa.  Second, numeric arguments of powers of 4 can be passed by
prefixing a command with control-U's.  Each control-U increases the
power by 1.  Hence control-U means 4, control-Ucontrol-U means 16,
control-Ucontrol-Ucontrol-U means 64 and so on.  Thus,
"control-Ucontrol-Ua" yields:aaaaaaaaaaaaaaaa.  If an explicit numeric
argument is not given, EMACS uses 1.  Note also that negative numeric
arguments can also be given and EMACS tries to behave intelligently
about them. 
 
     The structure of this document is functional: i.e., each
section is devoted to a set of common activities such as moving
around, deleting, copying etc.
@chapter(PRELIMINARIES)
@section(REGION or POINT and MARK)

     A region is a block of text to be acted upon by some EMACS
commands.  It is demarcated by the POINT on one end and the MARK at
the other.  The point is the primary location identifier where most
of the action takes place and is always @i(between) two characters. 
The point is indicated by the cursor position in that it is just
behind the cursor.  The point is also significant in that it defines
one end of the region.  The mark, on the other hand, is invisible,
and is used to demarcate the other end of the region( see section
@ref(regmov) or, to peek ahead, one denotes the mark by escape .). 
 
@section(BUFFERS)
 
     Buffers are regions of memory which are the primary units internal
to EMACS and where EMACS does its work. Thus, a file being edited
is located in a buffer. One can have multiple buffers, each with
a unique name, each of which can contain a file to be worked
upon.  The KILL buffer is a very special buffer that is always around
and which holds large pieces of deleted text.  While you cannot
directly work with the buffer, you use it indirectly in copying/moving
text segments.

@section(MODE)
 
     An EMACS mode specifies the general fashion in which EMACS
deals with user input.  For example, how should EMACS deal with long
lines (> 79 characters)? The mode is associated with the buffer and
not the file.  Hence, the mode has to be explicitly set (or removed)
each time the file is to be edited.  However, since the mode is
associated with the buffer, a new file read into the buffer will be
edited with the previously specified mode.  Also, standard (default)
modes can be set for all buffers when EMACS is first started.  (See
@ref(modes) for more information of modes.)
 
@section(WINDOWS)
 
     Windows are the visible portions of buffers. EMACS allows as
many windows on the screen as can fit. Since windows are screen
objects, the relationship of windows to buffers can be
many-to-one. That is, one can have many windows per buffer or a
each window could be into a different buffer.
 
@section(SCREEN)
 
     The screen is usually the 24 row by 80 column section of the
physical screen that is available to EMACS.  The format is as
follows.  The 24th row (the bottom line) is generally clear.  It is
used for EMACS prompts, user responses, and EMACS messages.  The
remainder of the screen (23 rows) is left for text display.  For
each window that is open on the screen, the bottom line is used to
provide information to the user.  Starting with the program's name
and version number, it contains the buffer mode(s) in parenthesis,
the buffer name and, finally, the file name associated with theref
buffer if there is one so associated.  Some computers may use a
different number of rows than normal (for example the HP110 portable
computer).
 
@section(OBJECTS)
 
     Objects are the entities upon which commands will operate. The most
common object is the character. Other objects are: words, lines, sentences,
paragraphs, screens, regions, and files.
 
@section(NOTATION)
 
      Standard visible characters are shown normally.  Control charactes
(the so-called non-printing characters) are shown prefixed by a @b(^)
e.g., control-A as ^A.  NOTE: in 8-bit ASCII there are 32 non-printing
characters: @center<^@ ^A..^Z ^[ ^\ ^] ^^ ^_>

	Control characters are case independent; i.e., ^A is the same as
^a.  Further, the escape key which, technically, is ^[ is denoted by
ESC; most keyboards have a special key marked ESC.  In two-keystroke
commands, the intervening blank space shown is for clarity and should
not be typed.  Parameters to be supplied by you are indicated as
[parameter-type].  Finally, special keys are denoted by <key-name>. 
 
@section(GETTING STARTED)

	To start using EMACS, just type @b(EMACS [filename]) (where
[filename] is the name of the file you wish to edit) from your operating
system's command prompt.  This will bring up EMACS and read in the file
you wish to edit.  (@i[If you are using EMACS for the first time one
your machine, read @ref<install> to learn how to install EMACS on your
machine]).

@chapter(INSERTING TEXT)
 
    All standard "printable" ASCII characters are self-inserting.  This
means for those of you who have "extended" characters (ASCII 128-255)
you can type these in just as you any other character.  For example, on
the IBM-PC, keeping the <Alt> key pressed down while typing in another
key sequence will give you the "extended" character.  With the following
exceptions, all other non-printing characters are commands:
 
@begin(description)
^I (or <TAB>)@/Handle-Tab: inserts the tab character or spaces to move
the currently set tab distance (see @ref(tabset)).  When you first start
EMACS, this character inserts itself and while you "see" spaces, there
is just one character there.  However, if you use the set-tab distance
command (by ESC [number] ^I) then the required tab distance is obtained
by inserting the requisite number of spaces and @i(now) the spaces that
you see are indeed spaces.  To switch back to using true tab characters,
use the set-tab distance command with a zero argument (ESC 0 ^I). 

^M (or <RETURN>)@/Insert-Newline.  insert a new-line at point.  Note
that this is a character like any other and can be deleted or inserted
at any place.  However it looks different because it acts like the
carriage-return line feed pair. 

^J (or <LINEFEED>)@/NewLine-Indent: like above but indent to previous
line's indentation.  Hence, a ^J actually inserts a new-line and as many
spaces as required to move under the first character on the previous
line. 

^O (oh)@/Open-Line: insert an open line at cursor for entering text. 
This is a very useful command when you are going to be inserting more
than a few characters in between existing characters.  By using ^O you
will not be forcing EMACS to shift other text around as you type and
things will both go faster and look cleaner on the screen.  If you are
going to insert a lot of new text, use a numeric argument to get a lot
of blank lines after point.  You can use ^X ^O to clean up the buffer
after you finish adding your text (see @ref(deblank)). 

^Q@/Quote-Next-Character.  If you wish certain control characters
(non-printing) to appear in your text, you can do so by "quoting" them
with a ^Q.  Hence the sequence: ^Q [char] will insert the [char] into
the text. 

^C@/Insert-Space.  This command inserts a space after point.  While not
particularly useful in general (since the space-bar "key" does the same
thing and more easily) it is very useful when you are in "OVERstrike mode"
(see @ref(overmode)).
@end(description)
@chapter(MOVING AROUND)
 
     In this chapter, we cover ways of moving around in a variety
of contexts: the screen, region, and file.
 
@section(SCREEN)
 
     Movement commands work on pieces of text called objects.  Three objects
are involved in moving around the screen: character, word and paragraph.
@subsection(Character Moving)
@begin(description) 
^F@/Forward-Charcter.  (right cursor on many machines) The point moves
forward by one character hence the cursor also moves since the cursor is
just after point.  Again note that since the new-line character is a
character, you can "move" over it just as you would any other character. 
On the screen this appears as a jump to the beginning of the next line. 

^B@/Backward-Character.  (left cursor) Just like Forward-Character but
in the reverse direction. 
@end(description)
@subsection(Line Moving)
@begin(description)
^P@/Previous-Line.  (up cursor) The point moves to the same column in
the previous line if that is possible.  If there is no text for it to
move to it moves to the end of the line.  However, it "remembers" the
column it was in and moves to it when you return to the starting line. 
If the point is on the top line of the screen, the screen scrolls
backward so that the previous line of the text is centered on the new
screen. 

^N@/Next-Line.  (down cursor) Like ^P but point moves to the next line.

^E@/End-Of-Line.  The point moves after the last character on the line
and stays between it and the <new-line> character.  Hence another ^F
(Forward-Character) would put you past the new-line, i.e., at the
beginning of the next line. 

^A@/Start-Of-Line.  The point moves to the beginning of the line and is
between the first character on the line and the new-line character of
the previous line.  A ^B (or Backward-Character), then, would put you at
the end of the previous line. 

ESC G@/To-Numbered-Line.  The cursor moves to the line specifed by the
numeric argument.  If none is given, EMACS, as usual, would assume 1 and
move you to the top of the buffer.  Use ESC <number> to denote the
target line where the first line is 1.  Hence, for example, if you
wished to go to the 83 line you would type ESC 83 ESC G. 
@end(description)
@section(Word Moving)
@begin(description) 

ESC F@/Forward-Word.  The point is moved from its current location to
the beginning of the next word.  A word is defined to be contiguous
characters separated by spaces or punctuation marks.  Hence if the point
is currently in middle of two words (i.e., @i[in front of the space or
the punctuation]) it is moved to the beginning of the next word. 

ESC B@/Backward-Word.  Like forward word but in the reverse direction.
@end(description)
@section(Paragraph Moving)
@begin(description)
ESC N@/Forward-Paragraph.  Move to the beginning of the next paragraph.

	NOTE: A paragraph is defined as a section of text separated from
other text by 1) blank lines, 2) a line starting with blanks, or 3) a
line starting with a tab character (^I).@tag(paradef)

ESC N@/Backward-Paragraph.  Like forward-paragraph but in the reverse
direction.
@end(description)
@section(Region Moving)
@TAG(regmov)
@begin(description)
^X ^X@/Exchange-Point-And-Mark.  There is only one region available at a
time.  Hence, moving by regions is restricted to moving from one end of
the region to the other.  To reiterate, the region is defined by the
point (behind the cursor) at one end and the mark (set by ESC .  or ESC
space) at the other.  Recall that @i<this> end of the region is where
the cursor is.  Hence exchanging point and mark effectively moves you
from one end of the region to the other. 
@end(description)
@section<Screen Moving>
@begin(description)
^V or <NEXT>@/Next-Screen.  The screen is scrolled up so that the next
set of lines can be shown in the window.  EMACS retains an overlap of
two lines for continuity -- i.e., you do not get a new screen starting
with the first previously invisible line but rather the new screen will
start with two lines from the bottom of the previous screen.  The point
is left at the home position (top left hand corner). 

ESC V or CTRL Z or <PREV>@/Previous-Screen.  Like above except that the previous
set of screen lines is displayed.
@end(description)
@section(Window Moving)
@begin(description)

^X O (oh)@/Next-Window.  Moves point to the the next window on the
screen.  EMACS cycles through windows.  Thus, if you are in the bottom
window, you are moved to the top window. 

^X P@/Previous-Window.  Like above but in the opposite direction.
@end(description)

For addtional information on windows, see @ref(windows).
@section(Buffer Moving)@begin(description)

ESC < or <HOME>@/Top-Of-Buffer.  Moves point to the very beginning of
the buffer.

ESC >or <shift-HOME>@/Bottom-Of-Buffer.  Moves point to the very end of
the buffer. 

ESC B@/Select-Buffer.  You will be prompted for a buffer name (see
@ref(buflist) on how to find out about your buffers) and will be
switched to it.  If the buffer does not exits, EMACS will create one
and put you into it.

^X X@/Next-Buffer.  You will be put into the next buffer in the buffer
list.  Like with windows, EMACS cycles through the buffer list and hence
if you are in the last buffer, you will be put into the first one. 
@end(description)

For additional information on buffers, see @ref(buffers).

@section(File Moving)
A file is a named collection of text and is associated with a buffer in
EMACS.  EMACS allows full filenames; hence the full path can be
specifed.  In all cases of moving between files, you will be prompted
for a filename Type it in and hit <return>.
@begin(description)

^X ^R@/Read-File.  The file, if found, is read into the current buffer
overwriting any text already in it.  If the buffer has been changed and
you have not saved your changes, you will be asked to confirm the
overwrite.  If the file is not found where specified, it is assumed that
you wish to create a new file with that name and your current buffer is
merely emptied. 

^X ^F@/Find-File.  EMACS attempts first to find the specified file
within one of the existing buffers.  If it finds the file it merely
switches you to that buffer.  If it does not, it will create a new
buffer, read the specified file if found into it, and switch you to the
new buffer.  If the file is not found, a new buffer is created and you
will be put into it. 

^X ^V@/View-File.  Like find-a-file above except that the VIEW mode is
automatically added to the buffer.  In this mode you are only allowed to
move around the buffer and cannot make any changes. 
@end(description)
 
@chapter(DELETING) 
Like moving around, deleting is an action that can work with a variety
of objects: characters, words, lines, paragraphs, regions, windows, and
buffers.  It is worth reiterating that the object is defined relative to
the point and not the object as a whole.  Hence, if you are in the
middle of a word and issue a delete-forward-word command, the text
deleted is actually from the point to the end of the word. 

@section(Character)
@begin(description)
CTRL-D@/Delete-Forward-Character.  Since the point is always just before
the cursor, the character under the cursor is deleted.  When you are at
the end of a line, the point is just before the <newline> character and,
therefore, a Delete-Forward-Character "eats" up the <newline> -- in
effect this joins the next line to the current one. 

CTRL-H  or <BACKSPACE>@/Delete-Backward-Character.  This deletes the
character before point.  At the beginning of a line, it deletes the
previous <newline> character and hence joins the current line to the
previous one.
@end(description)@section(Word)@begin(description)
ESC D@/Delete-Forward-Word.  Deletes the text from the point to the
@i<beginning> of the next word.  Hence, any punctuation separating the
words is also deleted.

ESC <backspace> or ESC CTRL-H@/Delete-Backward-Word.  Like above but
in the reversse direction.
@end(description)@section(Line)@begin(description)
CTRL-K@/Delete-To-End-Of-Line (Kill line).  All characters from point to the end
of the line are deleted.  @i[The <new-line> character is NOT deleted.]

	To delete a line completely from the buffer you would have to do the
following: CTRL-A (Start-Of-Line) CTRL-K (Delete-To-End-Of-Line) CTRL-D
(Delete-Forward-Character to delete the <newline> character).

@tag(delete)
As with all major deletes (i.e., when more than a few characters are deleted
EMACS places the deleted text in a KILL buffer.  Hence, the deletion
can be undone by "yanking" the text back at point (using CTRL-Y).

CTRL-X CTRL-O (oh)@/Delete-Blank-Lines.  This command deletes all blank
@i<lines> around the current line.  If there is only blank line after the
current line it is @i<not> deleted.  However, if there are more than 1
blank lines after the current line, @i<all> of them are deleted.@tag(deblank)
@end(description)@section(Region)
The region is an arbitrary area of the buffer demarcated by the point 
(just before the cursor!) and the mark. The mark is invisible and is
set using ESC . or ESC <space>.  To see the other end of the region,
use the Exchange-Point-And-Mark (CTRL-X XTRL-X) command.  Since the
region has no direction associated with it, this effectively shows you
the mark and all region commands work correctly.  Furthermore, since
the region is completely arbitrary in size you could demarcate any
sized object from a single character all the way to the entire buffer.
@description<
CTRL-W@/Delete-Region (kill World!).  Deletes the region (between
point and mark) and places the killed text in the KILL buffer to be "yanked"
back (with CTRL-Y) at the point.  Hence if you ever accidentally type CTRL-W
simply undo it with an immediate CTRL-Y (strictly speaking this does
not have to be done immediately since the KILL buffer is overwritten
only with the next big delete).>
@section<Windows>@description<
CTRL-X 1 (one)@/Close-All-Other-Windows.  This will close all windows on the
screen except for the one you are in (issued the command from).>
@section(Buffers)@description<
CTRL-X K@/Delete-Buffer.  You will be prompted for the name of a buffer
If the buffer has been changed, you will be asked to confirm the
deletion since killing the buffer would eliminated your changes.  To
find out which buffers you have, use the CTRL-X CTRL-B command (see
@tag<buflist>).  As always CTRL-G aborts the pending command -- which
means @i(before) you hit return after typing in the buffer name.>

@section<Files>
 
     Files within EMACS are dealt with by dealing with the associated
buffers.  If you really want to deal with the disk based versions of
files, escape to MSDOS, manage your files and return to EMACS (see
@ref(msdos) for a discussion on using MSDOS from within EMACS). 

@chapter(MOVING/COPYING TEXT)
  
     Transposing single characters is easy in EMACS.  CTRL-T does it. 
Larger entities are dealt with via the KILL buffer (see @ref(delete)). 
Moving or copying text in EMACS is fundamentally identical.  The general
procedure is as follows:
@begin(enumerate) 

If pre-defined areas of text are the desired areas to move or copy use
the appropriate delete command (delete-line,  delete para etc.) to get
a copy of the area into the KILL buffer.  An immediate "yank" (CTRL-Y)
results in an unchanged buffer.

If arbitrary areas of text are desired, the region is used.
Demarcate the region to be moved/copied (to repeat:
at one end of the region use the set-mark command (with ESC .
or ESC <space>) and move the cursor to the other end.  Delete (CTRL-W for
move) or copy (ESC W) the region into the KILL buffer.
 
Once the desired text is in the KILL buffer, it can be inserted into
@i(any) buffer by moving the point into the desired buffer at the desired
location and "yanking" (CTRL-Y) the text from the KILL buffer.
@end(enumerate)@begin(description)
CTRL-W@/Kill-Region.  Delete text between point and mark and copy it
into the KILL buffer.

ESC W@/Copy-Region.  Copy text between point and mark into the KILL buffer.

CTRL-Y@/Yank-Text.  Insert a copy of the KILL buffer into current
buffer at point.  @i<If you accidently typed CTRL-W (the kill region command),
immediately recover by typing in CTRL-Y (the yank text command).>
@end(description)

Since the KILL buffer is an independent buffer, you can move text
between @i<files> by moving text between @i<buffers>.  Further, "yanking"
is a read-only operation; hence the text can be inserted as many
times as desired with repeated yanks.  However,  the KILL buffer gets
overwritten with each major delete (i.e., deletion of more than a few
characters at a time).  Consequently you cannot (as yet) accumulate
text in the KILL buffer with a sequence of kill.  If you need to
accumulate text use a regular buffer (one that you ask for, see @ref(buffers))
to hold copies of the deleted (or copied) text.  If you are doing a
lot of accumulation, you might want to use a keyboard macro (see
@ref(kbdmacro)).
@chapter(SEARCH/REPLACE)
 
Searching and replacing occurs from the current cursor position towards
the end of the buffers.  When either (forward or backward) search type
is asked for, you will be prompted for the search string on the prompt
line.  Type in the string (including carraige returns, control
characters etc.) and finish by hitting the ESC key which marks the end
of the string.  To use the ESC character as part of a string, precede it
with a ^Q (quote) command.  ^Q can also be inserted by using two ^Qs. 
The seach behaviour of EMACS depends upon the setting of the exact mode
of the buffer (see @ref(buffers) for details on modes).  If exact mode
is on, then the search is done on a case sensitive basis.  Otherwise,
the case of the target is ignored.  Note that the default for every
buffer is for the exact mode to be off (unless the global mode is set to
have exact on for each buffer, see @ref(globalmode).  Obviously, no such
constraint exists for the replacement string since the replacement is
@i<always> exact.  EMACS remembers the search and replace strings and
always shows you the current stored string when it prompts you for a
string .  This string is used if you respond to the prompt with a null
string (i.e., if you simply hit ESC when prompted). 

There are two ways to replace strings in EMACS: unconditional or query
replace.  In the first, EMACS moves from point to the end of the buffer
replacing @i<every> occurrence of the search string with the replacement
string without pause.  In the second, EMACS pauses at every search
string found and prompts you for an action command which allows you to
control exactly which occurrences of the search string will be replaced. 
Because of the lack of control over the regular replace, it is strongly
recommended that you be very sure of your changes before you use it.  In
general, the query replace command should be used.  Procedurally,
replacing strings with other strings (including the null string) is done
as follows: first you are prompted for the search string and then for
the replacement string.  EMACS then moves from the current position to
the end of the buffer replacing (or querying) as it goes along.  Note
that there is no control over the direction of the replacement -- EMACS
always moves foward.  Hence, to work on the entire buffer, you need
first to move to the top of the buffer and issue the replace command. 
@begin(description)

CTRL-S@/Search-Forward.  Search for input string (stored string if
input string is null) from point towards the end of the buffer.  The
point is moved from the current location to the end of the found string.

CTRL-R@/Search-Backward.  Like forward except in the opposite direction
i.e., towards the beginning of the buffer.  Note that there is no
distinction made between forward and backward stored search strings.  Finally
if the search string is found, the point is moved to the beginning of
the string.

ESC R@/Replace.  Replace @i<ALL> occurrences of the search string with
the null string from point to end-of-buffer.

ESC CTRL-R@/Query-Replace.  Like above, but pause at each search
string found and query for action.
The response expected from you and their effects are as follows:
@begin(description)
Y(es) or <space>@/Make this replacement
 
N(o)@/Do not make this replacement but continue
 
!@/Do the rest of the replacements with no queries
 
CTRL-G@/Abort the replacement command.  NOTE:  This does not
undo any previous replacement that you had authorized.
 
.@/Exactly like CTRL-G except that the cursor returns to
the point at which the replacement command was given.
 
?@/Help for query replacement
@end(description)
@end(description) 

@chapter(DEALING WITH WINDOWS)

@tag(windows)First of all, note that windows are literally that: they
are areas of text in a buffer that you can see on the screen.  On the
other hand, by moving between windows and visiting different buffers in
each, you can effectively edit several files at the same time.  When you
ask for additional windows, EMACS splits the @i<current> window into two
and leaves you in the window created over the current pointer.  Thus you
can get multiple windows on the screen.  What is less obvious is that
you are always in a window which means that all window commands operate
even when you are in a single window.  Each window is similar in that it
has a text area and the mode line.  However the information/prompt line
is common to all windows (and buffers).  Further the new window will be
into the same buffer as the one from which the create window command was
given.  To move the window to another buffer, you will have to "find"
the desired buffer from the desired window.  Since files are associated
with buffers, this lets you simultaneously edit several files.  Which
leads us to the final point: all commands issued to EMACS are executed
on the current buffer in the current window.
@begin(description)

CTRL-X O(oh)@/Next-Window.  The current point is moved into the next
(i.e., towards the end of the screen) window.  Note that the editor
cycles through windows which means that if the command is issued from
the bottommost window you are put into the top window.

CTRL-XP@/Previous-Window.  Like above but works on the previous
window. 

CTRL-X 2@/Split-current-window.  The current window is split into two
windows -- @i<IF> there is enough space on the screen (a minimum of 1
line of text and 1 mode line per window are required).

CTRL-X 1@/Delete-other-windows.  This closes @i<all> windows except the one
from which you issued the command.
 
CTRL-X CTRL-N@/Move-window-down.  This scrolls the current window down
one line; i.e., the top line of the window dissappears and the hitherto
invisible "next" line becomes visible.  Hence the scroll window commands
work counter-intuitively in that ordinarily one moves ones head with a
"real" window or one moves the paper while here one is literally moving
the window on to the buffer. 

CTRL-X CTRL-P@/Move-window-up.  Like above except in the opposite
direction; i.e., the previously invisible line towards the beginning of
the buffer is made visible and the bottom line dissappears from the
window. 

ECS ! or ESC CTRL-L@/Redraw-display.  The window is moved such that the
line with the point (with the cursor) is at the center of the window.
 
CTRL-X ^ or CTRL-X Z@/Grow-Window.  The window with the pointer is
enlarged by one line and the nearest window is shrunk by one line. 

CTRL-X CTRL-Z@/Shrink-Window.  Like above but the current
window is shrunk and the nearest window is enlarged.
 
CTRL-L@/Clear-and-redraw.  The screen is blanked and redrawn.  Useful if
the screen updates are out of sync with your commands (Caused by DOS
disk errors, or system messages on mainframe computers). 
@end(description)

@comment(chapter9to12)
@chapter<DEALING WITH BUFFERS>@tag(buffers)

     Buffers are the major internal entities in EMACS and are
characterized by three things: their names, their modes, and the file
with which they are associated.  Furthermore, each buffer has its own
remembered mark and point which makes it easy to "visit" other buffers
and return to the original location in the "current" buffer.  Dealing
with them requires the following commands:

@begin(description)
ESC < or <HOME>@/Beginning-of-buffer.  Moves point to the very beginning of
the buffer.

ESC >or <shift-HOME>@/End-of-file. Moves point to the very end of the buffer.

CTRL-X B@/Select-Buffer.  You will be prompted for a buffer name (see
@ref(buflist) on how to find out about your buffers) and will be
switched to it.  If the buffer does not exits, EMACS will create one and
put you into it. 

CTRL-X X@/Next-Buffer.  You will be put into the next buffer in the
buffer list.  Like with windows, EMACS cycles through the buffer list
and hence if you are in the last buffer, you will be put into the first
one. 
 
CTRL-X CTRL-B@/List-Buffers.  A new window is created into the "List"
buffer which contains details about all the buffers currently known to EMACS.
You are left in the original buffer.  To close the new window issue
the close-windows command (CTRL-X 1).  The "List" buffer contains
information about the set global modes, the "buffer changed" indicator
(an asterisk in the 2nd column), the buffer specific modes, the
buffer size, the buffer name, and the associated filename.
 
 
CTRL-X K@/Delete-Buffer.  The specified buffer (as above) is
deleted if found.  Since this is a destructive operation, you will
asked for confirmation if the buffer was changed and not saved. 
Answer Y(es) or N(o).  As usual, CTRL-G cancels the command.
@end(description)

@section(Modes) @tag(modes)
     Modes are associated with buffers and govern the way EMACS reacts
to certain commands (to be described below).  Each buffer starts with
no modes set and this is indicated on the information line in parentheses
However, you can set "global" modes which means that each new buffer
starts off in the set global modes.  Alternatively, you can add and
delete modes for each buffer separately.

Currently, EMACS has the following modes: WRAP, VIEW, OVERwrite, EXACT,
and CMODE.  Note that the default for all buffers is to be placed with
no modes unless global modes are set.  In this normal case, long lines
are not folded and the line shifts to the left to enable you to view
lines longer than 79 characters.  A dollar sign in the last column
indicates a line that is longer than 79 characters.  Furthermore, any
text typed after the 79th character is inserted exactly as is thus
enabling you to enter very long lines into your buffer.  Also no other
modes are set and hence you have no wrapping, insert mode, and inexact
match.
@begin(description)

Wrap@/When wrap mode is set, EMACS tries to fold (break) lines at
the currently defined right margin. See @tag(wrap) for more details on
wrapping text. (Also see the fill-paragraph command for more assistance
on formating text).

View@/View mode essentially puts you into a read only buffer to prevent
accidental damage to files.  Some special files, such as the help file,
are automatically put into view mode buffer.@tag(viewmode)

Over@/In OVERwrite mode, the normal "insert" mode is toggled off which
means anything you type in that would normally have inserted itself
would now overwrite the existing text.  The Insert-Space command
(currently bound to CTRL-C) is very useful in this mode since it is
self-inserting.@tag(insertmode)

Exact@/The exact mode controls the manner in which string searches are
done: if exact mode is set, the search is case sensitive; if it is off,
the case of the target is ignored. 

CMODE@/This mode is automatically set if the file extension is .c or .h and
does useful things for you when you are writing programs in C. For
example, lines are automatically indented properly as you enter new C
code and braces and parenthises are matched when the appropriate close
is typed.
@end(description)


The commands required to deal with modes are as follows:
 
@begin(description)
CTRL-X M@/Add-Mode.  You will be prompted for the mode to add.  Type it
in and hit return.  The mode line will change to reflect the addition.
As always, CTRL-G gets you out.  NOTE: To set the right margin when in wrap
mode, use the set right margin command with the appropriate numerical prefix
argument with ESC <number> CTRL-X F.  Thus, for example, to set the right
margin at column 68 you would type:  ESC 68 CTRL-X F.  (See @ref<setmargin>).

CTRL-X CTRL-M@/Delete-Mode.  Like above but to remove a mode for the current
buffer.
 
ESC M@/Add-Global-Mode.  The specified global mode is marked as to-be-added
to any new buffer.  Hence, the modes for existing buffers do not
change.@tag(globalmode)
 
ESC CTRL-M@/Delete-Global-Mode.  The specified global mode is removed
from the modes to be added to new buffers.   Once again, existing
buffers and their modes are not effected.
@end(description)

@chapter(DEALING WITH FILES)
Text files are usually thought of as named collections of text residing
on disk (or some other storage medium).  In EMACS the disk based
versions of files come into play only when reading into or writing out
buffers.  The link between the physical file and the buffer is through
the associated filename.  EMACS permits full filenames; i.e., you can
specify: disk:\directories\filename.extension.  If the disk and
directories are not specified, the default disk is used.  Because of the
way EMACS deals with files, several points ought to be noted.  First,
without explicitly saving the buffer into a files, all your edits would
be gone upon leaving EMACS (you are asked to confirm whenever you are
about to lose edits).  Second, EMACS (at least currently) has no
mechanism for "protecting" your diskbased files from overwriting when it
saves files.  When instructed to save a file, it merrily proceeds to
dump the file to disk.  If it didn't previously exist it is now created. 
If it did, it is overwritten and the previous version is lost for ever. 
Hence, if you are unsure of your edits or for any other reason wish to
keep older versions of files around, the safe procedure is to read the
file into a buffer (with CTRL-X R), immediately change the associated
file name (with CTRL-X N), and then start your edits.  However, if you
do not wish to do any edits but merely peruse the file, add the view
mode (see @ref(viewmode)) to the buffer or ask for the file to be read
in for viewing only (with CTRL-X V).  The following are the file related
commands in EMACS.
@begin(description)
 
CTRL-X CTRL-S@/Save-file.  Saves the contents of the current buffer with
the associated filename on the default disk/directory (if not
specified).  Note that CTRL-X S also works. 
 
CTRL-X CTRL-W@/Write-file.  You will be prompted for a file name.  Type
it in and hit <return>.  The buffer contents will be saved under the
given name. 
 
CTRL-X N@/Change-filename.  The associated filename is changed (or
associated if not previously specified) as specified. 

CTRL-X CTRL-F@/Find--file.  You will be prompted for a filename.  If the
file has already been read into a buffer, you will be switch to it.  If
not, it will be read into a new buffer and you will be put into that
buffer. 
 
CTRL-X CTRL-R@/Read-File.  You will be prompted for a filename.  If the
file has already been read into another buffer,  you will be switched to it.
If not,  it will be read into the CURRENT buffer thus overwriting the
buffer contents.  As always, you will be asked for confirmation of the
overwrite if the buffer has been changed since the last save.
 
CTRL-X CTRL-V@/View-File.  Exactly like the above except that the
buffer will automatically be put into view mode thus not allowing you
to make any changes to it. 
@end(description)
@chapter(FORMATTING)
 
     While, as said before, EMACS is not a word-processor, some
formatting facilities are available. In no particular order they
are:
 
@section(WRAPPING TEXT)
 
    Normally, EMACS allows you to type in long lines (longer than
the screen width). However, if you wish it to automatically wrap
lines longer than a given width, you can do so by setting the
@tag(setmargin)
WRAP mode.
@begin(description) 
CTRL-X M [WRAP]@/Add-Wrap-Mode.  Add wrap mode to current buffer. 
Note that [WRAP] means that you respong with WRAP when prompted.

CTRL-X CTRL-M [WRAP]@/Delete-Wrap-Mode.  Removes wrap mode from
current buffer.
@end(description)

Wrap mode does not set the column (margin) at which wrapping is
supposed to occur.  Hence, it is very important that along with
adding wrap mode to the buffer, you set the desired margin.  If you don't,
the usual default value of 72 is used.

@begin(description)
CTRL-X F@/Set-Fill-Column.  Sets the wrap margin to the given numeric argument
If you do not precede this command with a numeric argument (with ESC <number>),
the right margin is set at column 1 which is the default numeric argument. 
@end(description)

When in wrap mode, you can re-format a paragraph after extensive
editing as follows:
@begin<description>
ESC Q@/Fill-Paragraph.  See @ref(paradef) for a discussion of
what is considered to be a paragraph in EMACS.
@end(description) 
@section(Changing Case)
Changing the case of the text in EMACS is also easy:

@begin(description) 
ESC U@/Upper-Case-Word.  The text from point to the end of the word is
changed to all uppercase.

ESC L@/Lower-Case-Word.  Like above except that text is changed to
lower case.

ESC C@/Capitalize-Word.  The first word after point is capitalized 
(that is the first letter only is uppercased).  This means that if you
issued the command from within a word, the character after point is
capitalized resulting in some wierd looking text.
 
ESC CTRL-U@/Upper-Case-Region.  All of the text between point and mark (i.e.
in the region) is capitalized.

ESC CTRL-L@/Lower-Case-Region.  All of the text in the region (between
point and mark) is lower-cased.
@end(description) 
@section(Miscellaneous)
@tag(tabset)
Setting tabs to arbitray widths is possible in EMACS but you must be
aware of a subtle difference that it makes to your file and hence to
your editing.  When you start EMACS, the tab width is set to the
default (usually every 8th column) for the tab character (CTRL-I).  As
long as you stay with the default, every time you insert the tab
character, a CTRL-I get inserted.  Hence, you logically have a single
character which might appear to be several spaces on the screen (or
the output) depending upon the column location of the tab character. 
This means that to remove the spacing you have to delete a @i<single>
character -- the tab character.

On the other hand, the moment you explicitly set the tab interval
(even if it is to the default value), EMACS handles the tab character
by expanding the character into the required number of spaces to move
you to the appropriate column.   In this case, to remove the spacing
you have to delete the appropriate number of spaces inserted by EMACS
to get you to the right column.

@begin(description)
CTRL-I@/Set-Tab-Interval.  The tab interval is set to the given
numeric argument.  As always, the numeric argument preceeds the
command.  Hence to get tabs every 4 spaces you would type in@*
@center[    ESC 4 CTRL-I]@*
or more generally: ESC [number] CTRL-I.
@end(description)
@chapter(GETTING OUT OF THINGS)
 
    Any pending commands can be aborted with CTRL-G. All this
does is cancel the command at its current level. Hence, for
example, part of the way through a query replace, the abort will
not undo the changes you have already allowed but will cancel the
remainder.
 
   Getting out of EMACS itself is possible in several ways:
@begin(description) 

CTRL-X CTRL-C.@/Hard-Abort.  This gets you out of EMACS and back to DOS.
However if there are changed buffers which have not been
saved, you  will be queried.
 
ESC Z@/Save-All-And-Exit.  This gets you out of EMACS but only @i<after>
all changed buffers have been saved under the current associated filenames
Hence this could be a very dangerous command if there changed
buffers which will overwrite files you wished left unchanged.  In
general avoid this command.
@end(description)

***********************************************************
***                  W A R N I N G                      ***
***                                                     ***
*** AGAIN, PLEASE NOTE THAT ALL CHANGED BUFFERS WILL    ***
***         AUTOMATICALLY BE SAVED WITH ESC Z           ***
***********************************************************

@comment(ch13 to 16) 
@chapter(Keyboard Macros)@tag(kbdmacro)
 
     A keyboard macro is a short hand way to repeat a series of
commands.  In effect, a "recording" is started, with CTRL-X (,  of the
sequence of keys that you hit when defining a keyboard macro.  The
recording is then repeated when you execute the keyboard macro. 
Hence, you could record any combination of character input and
commands you like.  Once you stop recording your keystrokes(with
CTRL-X ) ), the entire sequence is available to you to be repeated
starting at the point at which the keyboard macro is invoked.  You
could, therefore start recording at some location, move to another
point, and repeat the entire sequence at that location by invoking the
recorded macro (with CTRL-X E). Since it is keystrokes that are being
saved, you can freely intermix commands and text to be inserted  into
the buffer.  Unfortunately, you cannot save a keyborad macro for
later.  If you start another keyboard macro recording session, the previously
defined macro is lost.  So make sure that you are done with the
current keyboard macro before defining another one.  If you have a
series of commands that you would like to "record" for later use, use
the execute-file or execute-buffer commands (see @ref(execbuf) and/or 
@ref(execfile)).
@begin(description)

CTRL-X (@/Start-Recording.  @b(All) keystrokes, commands and input are 
recorded till the end-recording command is given.

CTRL-X )@/End-Recording.    Stop recording keystrokes for macro.
CTRL-X e@/Execute-Macro.  The entire sequence of recorded keystrokes
is repeated starting at the current point.  The result is exactly as if you 
we retyping the sequence all over again.  A numeric argument prefixing
the Execute-Macro command repeats the stored keystrokes that many times.
@end(description) 
 
@chapter(Msdos And Emacs)@tag(msdos)
 
    There are two commands which will allow you to use MSDOS
while within EMACS. Procedurally, you will need to make sure that
the "current" directory contains COMMAND.COM before you issue any
internal commands (see the MSDOS manual). Also, external commands
or program may be run with some restrictions depending upon the
degree to which the program "takes over" the computer or the amount of
memory you have and the program size.  When you
use either of the commands, EMACS pops you into MSDOS.  The key
difference between the two commands is in how they act after the first
MSDOS command has finished executing.  If you use the Run-DOS-Command command
a <RETURN> will put you back in EMACS exactly as you were before. 
The Run-CLI (run the Command Line Interpretor), on the other hand,
leaves you at the DOS command level (usually the A> prompt) so that
you can continue issuing other commands; to return to EMACS, you need
to type in EXIT to leave the command line interpretor.
NOTE: since EMACS is callable from the command line, you could easily have
several EMACS images in memory!  In general, this is a practice to be avoided

@begin(description)

CTRL-X !@/Run-DOS-Command.  Suspends EMACS and, if the file COMMAND.COM is
on the default drive,  allows one DOS command to be executed.  After
finishing with that command, a <RETURN> will put you back in EMACS.

CTRL-X C@/Run-CLI.  Like above execpt that multiple commands are
permitted.  To return to EMACS, type in EXIT at the DOS prompt.
@end(description)

SUGGESTION: If you are going to run a program (or external MSDOS
command) from within EMACS for the first time, save your changed
buffers (if you so desire) before you start. Once you are sure
that the program will not damage EMACS, you need not take such
precautions. As a favor to the user community, please inform the
Computing Center of program that do and do not work and we will
try to disseminate the information.
 
PROGRAMS AND EXTERNAL COMMANDS TESTED SO FAR: 
XDIR@*
PRINT@*
KERMIT@*
FINALWORD@*
ALL "INTERNAL" COMMANDS@*

@chapter(Unbound Commands)
In this chapter we mention several commands that are not currently
bound to any keys.  If you find that you are using some of these
consistently, you might want to bind them yourself either permanently
or temporarily (see @ref(customization)).

To execute an unbound (or for that matter a bound command) use the
command: Execute-Named-Command which is currently bound to ESC-X.
@description[
ESC-X@/Execute-Named-Command.  You will be prompted (with a colon) on
the prompt line for the named command to be executed.  Appendix A
contains a list of the named comands that you could execute.  EMACS
attempts to complete the command if you hit the <SPACE> bar; i.e., if
the information that you have typed in so far is sufficient to
identify the command uniquely, EMACS finishes typing it in for you and
then waits for the <RETURN> before executing the command.]

Unbound commands are:
@begin(description)
Hunt-Forward@/Repeat the last forward search command. Thus this is
identical to you typing in CTRL-S (search-forward) and then defaulting
the search string (by simply giving an ESC at the prompt line).

Hunt-Backward@/Repeat the last reverse search command. Thus this is
identical to you typing in CTRL-R (search-reverse) and then defaulting
the search string (by simply giving an ESC at the prompt line).

Execute-Buffer@/You will be prompted for a buffer name.  If the buffer
exists, EMACS will assume that its contents are commands to be
executed by it before returning control to you.  If any of the
commands are incorrect in the buffer, EMACS aborts the command and
returns control to you immediately.  The syntax of commands in the
buffer is as follows:@*
	[numeric argument] named-command [string argument] [string argument]@*
where the only required parameter is the named-command itself.@tag(execbuf)

Execute-File@/Very similar to the Execute-Buffer command except that
you will be prompted for a file containing commands to be executed by
EMACS. As in the above case, error will cause an immediate abort and
the syntax of the commands is the same.@tag(execfile)

Execute-Command-Line@/????
@end(description)
@chapter(Customization)
@tag(customization)
EMACS is extensively customizable in that the keystroke used to invoke
a command can be changed to suit your needs at will.  The connection
of a keystroke to a command is called key-binding.  Keybinding can be
done temporarily or permanently -- by which we mean that the changed
keybinding will last only during one editing session or will always be
effective.  Furthermore, any keybinding can be removed permanently or during
an editing session.  We will cover temporary bindings and unbindings first.  

@begin(description)
ESC K@/Bind-To-Key.  The Bind-To-Key command, currently bound to ESC
K, will prompt you for the named command and the key to which it is to
be bound.  The set of named commands is in Appendix A.  The key(s) to
which the command is to be bound are simply typed in exactly.  That
is, to bind a command to, say ESC D, type in the named command, a
space, and then the keys ESC and D.

ESC CTRL-K@/Unbind-Key.  This command undoes the effect of the
previous command or any built-in bindings.  As above you will be
prompted for the required information -- in this case the information
is simply the key to be unbound which is to be typed in exactly as
above.
@end(description)
@i<@b[
NOTE:  be very careful in binding and unbinding keys since you could
get into some very peculiar situations such as being unable to abort
our of a command (if you unbind CTRL-G or bind it to something else)
or recover from the bad binding/unbinding if you unbind Execute-Named-Command
or the Unbind-Key command.  As long as you leave yourself the
opportunity to do either of the last two commands, youc an recover
from disasterous bindings/unbindings.]>

Permanent changes are done indirectly through the EMACS.RC file.  This
is a file that EMACS reads and executes (see @ref(execfile))
@i<before> startup and hence results in the appearance of a permanent
change in the keybindings.  The syntax of commands in the EMACS.RC
file is described under the Execute-File command (@ref(execfile)).  Of
principal concern here are the two commands Bind-To-Key and
Unbind-Key.  The primary difference between the way parameters are
passed to these commands in the EMACS.RC file is that the keys are not
typed in directly (as in the control-I key when you want CTRL-I) but
symbolically using the following symbols:

@description[

^@/for control keys. For example to indicate control-I, you would type ^I.

M@/for the escape key.  For example, to indicate ESC CTRL-K, you would
type in M^I.

FN@/for "function" keys.  The reason for the quotes is that for the
HP150 the term function is expanded to include all of the special keys
such as the <INSERT-CHAR> key.  
]
