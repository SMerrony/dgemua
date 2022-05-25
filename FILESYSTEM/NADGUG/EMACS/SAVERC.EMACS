;	EMACS.RC:	Startup file for MicroEMACS 3.4
;
;			This file is executed everytime the
;			editor is entered

;	***** Rebind the Function key group

bind-to-key execute-named-command	FN;
bind-to-key execute-file		FN<
bind-to-key hunt-forward		FN=
bind-to-key hunt-backward		FN>
bind-to-key next-window			FN?
bind-to-key execute-macro		FN@
bind-to-key find-file			FNA
bind-to-key view-file			FNB
bind-to-key save-file			FNC
bind-to-key exit-emacs			FND

;	set screen colors
add-global-mode "blue"

;	Enter Help
1 store-macro
	help
	8 resize-window
	bind-to-key execute-macro-12 FNI
	bind-to-key execute-macro-13 FNQ
	bind-to-key execute-macro-11 FNT
	add-mode "red"
	begining-of-file
	2 forward-character
	clear-message-line
[end]

;	consult file
2 store-macro
	2 split-current-window
	previous-window
	view-file "@File to Consult: "
	8 resize-window
	add-mode "green"
	add-mode "Black"
	next-window
[end]

;	Exit Help
11 store-macro
	bind-to-key previous-page FNI
	bind-to-key next-page FNQ
	bind-to-key execute-macro-1 FNT
	delete-window
	clear-message-line
[end]

;	last help page
12 store-macro
	begining-of-line
	search-reverse "=>"
	1 redraw-display
[end]

;	next help page
13 store-macro
	begining-of-line
	2 forward-character
	search-forward "=>"
	1 redraw-display
[end]

;	bring up the function key window

	2 split-current-window
	select-buffer "Function Keys"
	add-mode "red"
	2 resize-window
	insert-string "F1 exec cmd    F2  exec file   F3 hunt       F4  back hunt     F5  next window"
	newline
	insert-string "F6 exec macro  F7  find file   F8 view file  F9  save file     F10 exit emacs"
	begining-of-file
	unmark-buffer
	next-window
