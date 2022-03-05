_start:
	; Prints hi three times
	CALL print_hi
	CALL print_hi
	CALL print_hi
loop:
	JP _start
msg:
	db "Hello, world!",0