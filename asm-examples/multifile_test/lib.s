print_hi:
	LD I, #0xF002
	LD V0, $72 ; H
	LD [I], V0
	LD V0, $105 ; i
	LD [I], V0
	LD V0, $10 ; newline
	LD [I], V0
	RET

print:
	LD V1, V2, I
	LD V0, [I]
	SNE V0, $0
	JP print1
	LD I, #0xF002
	LD [I], V0
	LD I, V1, V2
	NXT
	JP print
print1:
	RET