_start:
	; Prints hi three times
	CALL print_hi
	CALL print_hi
	CALL print_hi
	LD I, msg
	CALL print
	LD I, msg2
	CALL print
loop:
	JP loop
msg:
	db "Hello, world!\n",0
msg2:
	db "This works!\n",0,"HAHA, no\n",0