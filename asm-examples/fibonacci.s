LD V0, $0 ; y
LD V1, $1 ; y
LD V2, $0 ; a

LD I, #0xF005 ; Serial NUMBER out
LD V2, V1 ; swap part 1
ADD V1, V0 ; Addition
LD V0, V2 ; swap part 2
LD [I], V0 ; Print
; Reset if overflow
SNE VF, $0
JP #0x000A
LD VF, $0
JP #26 ; loop