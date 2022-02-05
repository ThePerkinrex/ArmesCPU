print_hi:
	LD I, #0xF002
	LD V0, $72 ; H
	LD [I], V0
	LD V0, $105 ; i
	LD [I], V0
	LD V0, $10 ; newline
	LD [I], V0
	RET