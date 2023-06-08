
       PROCESSOR 6502
       INCLUDE "vcs.h"
       INCLUDE "macro.h"

;------------------------------Variables----------------------------------
       SEG.U Variables
       ORG $80
Counter              ds 2
TextLineCounter      ds 1
TextBlockPointer     ds 2
Char1Ptr             ds 2
Char2Ptr             ds 2
Char3Ptr             ds 2
Char4Ptr             ds 2
Char5Ptr             ds 2
Char6Ptr             ds 2
Char7Ptr             ds 2
Char8Ptr             ds 2
Char9Ptr             ds 2
Char10Ptr            ds 2
Char11Ptr            ds 2
Char12Ptr            ds 2
Temp                 ds 1
;-------------------------End Variables-----------------------------------
       SEG Bank0
       ORG $F000
Start
       CLEAN_START
       LDA #$EE
       STA COLUP0
       STA COLUP1
       LDA #$90
       STA COLUBK
       LDA #$70
       STA PF0
       LDA #$00
       STA PF1
       LDA #0
       STA PF2
       LDA #1
       STA CTRLPF
;-------------------------------------------------------------------------
;--------------GAME MAIN LOOP---------------------------------------------
;-------------------------------------------------------------------------
MainGameLoop
       JSR VBLANKRoutine
       JSR KernelRoutine
       JSR OverscanRoutine
       JMP MainGameLoop
;-------------------------------------------------------------------------
;-------------------VBLANK Routine----------------------------------------
;-------------------------------------------------------------------------
VBLANKRoutine
       LDA #%00000111
VSYNCLoop
       STA WSYNC
       STA VSYNC
       LSR
       BCS VSYNCLoop
       LDA #(36*76)>>6
       STA TIM64T
       DEC Counter
       BNE nohiclick
       DEC Counter+1
nohiclick:
       LDA #1
       STA VDELP0
       STA VDELP1
       LDA #%110
       STA NUSIZ0
       STA NUSIZ1
       LDA #0
       STA TextLineCounter
       JSR SetTextPointersSubroutine
       LDA #$15
       LDX #0
       JSR PositionASpriteSubroutine
       LDA #$25
       LDX #1
       JSR PositionASpriteSubroutine
WaitForVblankEnd
       LDA INTIM
       BNE WaitForVblankEnd
       STA WSYNC
       STA VBLANK	;turn off VBLANK - it was turned on by overscan
       RTS
;-------------------------------------------------------------------------
;----------------------Kernel Routine-------------------------------------
;-------------------------------------------------------------------------
KernelRoutine
       ; start timer for display period
       LDA     #(191*76)>>6
       STA     TIM64T

       LDA #$00
       STA TextLineCounter
       LDA #<TextBlock
       STA TextBlockPointer
       LDA #>TextBlock
       STA TextBlockPointer+1
       JSR SetTextPointersSubroutine
DrawAnotherLineOfText
       JSR DrawLineOfTextSubroutine
       LDA TextLineCounter
       CLC
       ADC #12
       STA TextLineCounter
       TAY
       LDA (TextBlockPointer),Y
       CMP #$FF
       BEQ TextEnd
       JSR SetTextPointersSubroutine
       JMP DrawAnotherLineOfText
TextEnd
       
WaitForKernelEnd
       LDA INTIM
       BNE WaitForKernelEnd
       RTS
;-------------------------------------------------------------------------
;------------------------Overscan Routine---------------------------------
;-------------------------------------------------------------------------
OverscanRoutine
       LDA #2
       STA WSYNC
       STA VBLANK	       ;turn on VBLANK
       LDA  #34
       STA  TIM64T
WaitForOverscanEnd
       LDA INTIM
       BNE WaitForOverscanEnd
       RTS
;-------------------------------------------------------------------------
;----------------------------End Main Routines----------------------------
;-------------------------------------------------------------------------

;*************************************************************************

;-------------------------------------------------------------------------
;----------------------Begin Subroutines----------------------------------
;-------------------------------------------------------------------------
       align 256
PositionASpriteSubroutine 
       SEC
       STA HMCLR
       STA WSYNC            ; begin line 1
DivideLoop
       SBC #15
       BCS DivideLoop       ; +4/5	 4/ 9.../54
       EOR #7			; +2	 6/11.../56
       ASL
       ASL
       ASL
       ASL			;+8    14/19.../64
       STA.wx HMP0,X		;+5	19/24.../69
       STA RESP0,X     	;+4    23/28/33/38/43/48/53/58/63/68/73
       STA WSYNC      	;+3    0      begin line 2
       STA HMOVE       	;+3
Ret
       RTS                  ;+6    9
;-------------------------------------------------------------------------
SetTextPointersSubroutine
       LDY TextLineCounter
       LDX #0
SetCharPtrsLoop
       LDA (TextBlockPointer),Y
       STA Char1Ptr,X
       LSR
       BCC OnCharSetPageOne
       LDA #>CharSetPageTwo
       STA Char1Ptr+1,X
       BNE DoneSettingCharPtrHi
OnCharSetPageOne
       LDA #>CharSetPageOne
       STA Char1Ptr+1,X
       NOP
DoneSettingCharPtrHi
       INX
       INX
       INY
       CPX #24
       BNE SetCharPtrsLoop
       RTS
;-------------------------------------------------------------------------
DrawLineOfTextSubroutine
LF303
       STA    HMCLR   
       STA    WSYNC   
       SLEEP 36		;+36	36
       LDX    #$90
       LDY    #8    		;+4	40
       LDA    Counter
       AND    #1    		;+5	45
       BEQ    SpritesLeft	;+2	47
       JMP    SpritesRight	;+3	50
LF327 
       STA    GRP1    	;+3	 9
       LDA    (Char5Ptr),Y 
       STA    GRP0    	;+8	17
       LDA    (Char7Ptr),Y 	;+5	22
       STX    HMP0    
       STX    HMP1    	;+6	28
       STA    GRP1    	;+3	31
       LDA    (Char9Ptr),Y 
       STA    GRP0    	;+8	39
       LDA    (Char11Ptr),Y 
       STA    GRP1    	;+8	47
       STA    GRP0    	;+3	50
SpritesRight
       DEY            
       BEQ    LF37D   	;+4	54
       LDA    (Char2Ptr),Y 
       LSR            
       STA    GRP0    	;+10	64
       LDA    (Char4Ptr),Y 
       LSR            
       STA.w  $001C   	;+11	75	GRP1, I assume.
       STA    HMOVE   	;+3	 2	sprites moved right
       LDA    (Char6Ptr),Y 
       LSR            
       STA    GRP0    	;+10	12
       LDA    (Char10Ptr),Y 
       LSR            
       STA    Temp     	;+10	22
       LDA    (Char8Ptr),Y 
       LSR            
       STA    GRP1    	;+10	32
       LDA    Temp     
       STA    GRP0          ;+6	38
       LDA    (Char12Ptr),Y 
       LSR            
       STA    GRP1          ;+10	48
SpritesLeft
       STA    GRP0    	;+3	51
       LDA    #$70
       STA    HMP0    
       STA    HMP1          ;+8	59
       DEY            
       BEQ    LF387   	;+4	63
       LDA    (Char1Ptr),Y 
       STA    GRP0    	;+8	71
       LDA    (Char3Ptr),Y 	;+5	76
       STA    HMOVE   	;+3	 3	sprites moved left
       JMP    LF327   	;+3	 6
LF37D
       STX    HMP0    
       STX    HMP1    
       STA    WSYNC   
       STA    HMOVE   
       BEQ    LF38C   
LF387
       STA    WSYNC
       NOP
       NOP
       NOP 
LF38C
       LDA    #0    
       STA    GRP0    
       STA    GRP1    
       STA    GRP0    
       RTS            
;*************************************************************************
;-------------------------------------------------------------------------

;-------------------------------------------------------------------------
;-------------------------Data Below--------------------------------------
;-------------------------------------------------------------------------
       ORG $F500
CharSetPageOne
A
       .byte $00 ; |        | $F0E0
       .byte $C6 ; |XX   XX | $F0E1
       .byte $C6 ; |XX   XX | $F0E2
       .byte $FE ; |XXXXXXX | $F0E3
       .byte $C6 ; |XX   XX | $F0E4
       .byte $C6 ; |XX   XX | $F0E5
       .byte $6C ; | XX XX  | $F0E6
       .byte $38 ; |  XXX   | $F0E7
B
       .byte $00 ; |        | $F0E8
       .byte $FC ; |XXXXXX  | $F0E9
       .byte $C6 ; |XX   XX | $F0EA
       .byte $C6 ; |XX   XX | $F0EB
       .byte $FC ; |XXXXXX  | $F0EC
       .byte $C6 ; |XX   XX | $F0ED
       .byte $C6 ; |XX   XX | $F0EE
       .byte $FC ; |XXXXXX  | $F0EF
C
       .byte $00 ; |        | $F0F0
       .byte $3C ; |  XXXX  | $F0F1
       .byte $66 ; | XX  XX | $F0F2
       .byte $C0 ; |XX      | $F0F3
       .byte $C0 ; |XX      | $F0F4
       .byte $C0 ; |XX      | $F0F5
       .byte $66 ; | XX  XX | $F0F6
       .byte $3C ; |  XXXX  | $F0F7
D
       .byte $00 ; |        | $F0F8
       .byte $F8 ; |XXXXX   | $F0F9
       .byte $CC ; |XX  XX  | $F0FA
       .byte $C6 ; |XX   XX | $F0FB
       .byte $C6 ; |XX   XX | $F0FC
       .byte $C6 ; |XX   XX | $F0FD
       .byte $CC ; |XX  XX  | $F0FE
       .byte $F8 ; |XXXXX   | $F0FF
E
       .byte $00 ; |        | $F100
       .byte $FE ; |XXXXXXX | $F101
       .byte $C0 ; |XX      | $F102
       .byte $C0 ; |XX      | $F103
       .byte $F8 ; |XXXXX   | $F104
       .byte $C0 ; |XX      | $F105
       .byte $C0 ; |XX      | $F106
       .byte $FE ; |XXXXXXX | $F107
F
       .byte $00 ; |        | $F108
       .byte $C0 ; |XX      | $F109
       .byte $C0 ; |XX      | $F10A
       .byte $C0 ; |XX      | $F10B
       .byte $FC ; |XXXXXX  | $F10C
       .byte $C0 ; |XX      | $F10D
       .byte $C0 ; |XX      | $F10E
       .byte $FE ; |XXXXXXX | $F10F
G
       .byte $00 ; |        | $F110
       .byte $3E ; |  XXXXX | $F111
       .byte $66 ; | XX  XX | $F112
       .byte $C6 ; |XX   XX | $F113
       .byte $CE ; |XX  XXX | $F114
       .byte $C0 ; |XX      | $F115
       .byte $60 ; | XX     | $F116
       .byte $3E ; |  XXXXX | $F117
H
       .byte $00 ; |        | $F118
       .byte $C6 ; |XX   XX | $F119
       .byte $C6 ; |XX   XX | $F11A
       .byte $C6 ; |XX   XX | $F11B
       .byte $FE ; |XXXXXXX | $F11C
       .byte $C6 ; |XX   XX | $F11D
       .byte $C6 ; |XX   XX | $F11E
       .byte $C6 ; |XX   XX | $F11F
I
       .byte $00 ; |        | $F120
       .byte $78 ; | XXXX   | $F121
       .byte $30 ; |  XX    | $F122
       .byte $30 ; |  XX    | $F123
       .byte $30 ; |  XX    | $F124
       .byte $30 ; |  XX    | $F125
       .byte $30 ; |  XX    | $F126
       .byte $78 ; | XXXX   | $F127
J
       .byte $00 ; |        | $F128
       .byte $7C ; | XXXXX  | $F129
       .byte $C6 ; |XX   XX | $F12A
       .byte $06 ; |     XX | $F12B
       .byte $06 ; |     XX | $F12C
       .byte $06 ; |     XX | $F12D
       .byte $06 ; |     XX | $F12E
       .byte $06 ; |     XX | $F12F
K
       .byte $00 ; |        | $F130
       .byte $CE ; |XX  XXX | $F131
       .byte $DC ; |XX XXX  | $F132
       .byte $F8 ; |XXXXX   | $F133
       .byte $F0 ; |XXXX    | $F134
       .byte $D8 ; |XX XX   | $F135
       .byte $CC ; |XX  XX  | $F136
       .byte $C6 ; |XX   XX | $F137
L
       .byte $00 ; |        | $F138
       .byte $FE ; |XXXXXXX | $F139
       .byte $C0 ; |XX      | $F13A
       .byte $C0 ; |XX      | $F13B
       .byte $C0 ; |XX      | $F13C
       .byte $C0 ; |XX      | $F13D
       .byte $C0 ; |XX      | $F13E
       .byte $C0 ; |XX      | $F13F
M
       .byte $00 ; |        | $F140
       .byte $C6 ; |XX   XX | $F141
       .byte $C6 ; |XX   XX | $F142
       .byte $D6 ; |XX X XX | $F143
       .byte $FE ; |XXXXXXX | $F144
       .byte $FE ; |XXXXXXX | $F145
       .byte $EE ; |XXX XXX | $F146
       .byte $C6 ; |XX   XX | $F147
N
       .byte $00 ; |        | $F148
       .byte $C6 ; |XX   XX | $F149
       .byte $CE ; |XX  XXX | $F14A
       .byte $DE ; |XX XXXX | $F14B
       .byte $FE ; |XXXXXXX | $F14C
       .byte $F6 ; |XXXX XX | $F14D
       .byte $E6 ; |XXX  XX | $F14E
       .byte $C6 ; |XX   XX | $F14F
O
       .byte $00 ; |        | $F150
       .byte $7C ; | XXXXX  | $F151
       .byte $C6 ; |XX   XX | $F152
       .byte $C6 ; |XX   XX | $F153
       .byte $C6 ; |XX   XX | $F154
       .byte $C6 ; |XX   XX | $F155
       .byte $C6 ; |XX   XX | $F156
       .byte $7C ; | XXXXX  | $F157
P
       .byte $00 ; |        | $F158
       .byte $C0 ; |XX      | $F159
       .byte $C0 ; |XX      | $F15A
       .byte $FC ; |XXXXXX  | $F15B
       .byte $C6 ; |XX   XX | $F15C
       .byte $C6 ; |XX   XX | $F15D
       .byte $C6 ; |XX   XX | $F15E
       .byte $FC ; |XXXXXX  | $F15F
Q
       .byte $00 ; |        | $F160
       .byte $76 ; | XXX XX | $F161
       .byte $CC ; |XX  XX  | $F162
       .byte $DA ; |XX XX X | $F163
       .byte $C6 ; |XX   XX | $F164
       .byte $C6 ; |XX   XX | $F165
       .byte $C6 ; |XX   XX | $F166
       .byte $7C ; | XXXXX  | $F167
R
       .byte $00 ; |        | $F168
       .byte $CE ; |XX  XXX | $F169
       .byte $DC ; |XX XXX  | $F16A
       .byte $F8 ; |XXXXX   | $F16B
       .byte $CE ; |XX  XXX | $F16C
       .byte $C6 ; |XX   XX | $F16D
       .byte $C6 ; |XX   XX | $F16E
       .byte $FC ; |XXXXXX  | $F16F
S
       .byte $00 ; |        | $F170
       .byte $7C ; | XXXXX  | $F171
       .byte $C6 ; |XX   XX | $F172
       .byte $06 ; |     XX | $F173
       .byte $7C ; | XXXXX  | $F174
       .byte $C0 ; |XX      | $F175
       .byte $CC ; |XX  XX  | $F176
       .byte $78 ; | XXXX   | $F177
T
       .byte $00 ; |        | $F178
       .byte $30 ; |  XX    | $F179
       .byte $30 ; |  XX    | $F17A
       .byte $30 ; |  XX    | $F17B
       .byte $30 ; |  XX    | $F17C
       .byte $30 ; |  XX    | $F17D
       .byte $30 ; |  XX    | $F17E
       .byte $FC ; |XXXXXX  | $F17F
U
       .byte $00 ; |        | $F180
       .byte $7C ; | XXXXX  | $F181
       .byte $C6 ; |XX   XX | $F182
       .byte $C6 ; |XX   XX | $F183
       .byte $C6 ; |XX   XX | $F184
       .byte $C6 ; |XX   XX | $F185
       .byte $C6 ; |XX   XX | $F186
       .byte $C6 ; |XX   XX | $F187
V
       .byte $00 ; |        | $F188
       .byte $10 ; |   X    | $F189
       .byte $38 ; |  XXX   | $F18A
       .byte $7C ; | XXXXX  | $F18B
       .byte $EE ; |XXX XXX | $F18C
       .byte $C6 ; |XX   XX | $F18D
       .byte $C6 ; |XX   XX | $F18E
       .byte $C6 ; |XX   XX | $F18F
W
       .byte $00 ; |        | $F190
       .byte $C6 ; |XX   XX | $F191
       .byte $EE ; |XXX XXX | $F192
       .byte $FE ; |XXXXXXX | $F193
       .byte $FE ; |XXXXXXX | $F194
       .byte $D6 ; |XX X XX | $F195
       .byte $C6 ; |XX   XX | $F196
       .byte $C6 ; |XX   XX | $F197
XX
       .byte $00 ; |        | $F198
       .byte $C6 ; |XX   XX | $F199
       .byte $EE ; |XXX XXX | $F19A
       .byte $7C ; | XXXXX  | $F19B
       .byte $38 ; |  XXX   | $F19C
       .byte $7C ; | XXXXX  | $F19D
       .byte $EE ; |XXX XXX | $F19E
       .byte $C6 ; |XX   XX | $F19F
YY
       .byte $00 ; |        | $F1A0
       .byte $30 ; |  XX    | $F1A1
       .byte $30 ; |  XX    | $F1A2
       .byte $30 ; |  XX    | $F1A3
       .byte $78 ; | XXXX   | $F1A4
       .byte $CC ; |XX  XX  | $F1A5
       .byte $CC ; |XX  XX  | $F1A6
       .byte $CC ; |XX  XX  | $F1A7
Z
       .byte $00 ; |        | $F1A8
       .byte $FE ; |XXXXXXX | $F1A9
       .byte $E0 ; |XXX     | $F1AA
       .byte $70 ; | XXX    | $F1AB
       .byte $38 ; |  XXX   | $F1AC
       .byte $1C ; |   XXX  | $F1AD
       .byte $0E ; |    XXX | $F1AE
       .byte $FE ; |XXXXXXX | $F1AF
_
       .byte $00 ; |        | $F1B0
       .byte $00 ; |        | $F1B1
       .byte $00 ; |        | $F1B2
       .byte $00 ; |        | $F1B3
       .byte $00 ; |        | $F1B4
       .byte $00 ; |        | $F1B5
       .byte $00 ; |        | $F1B6
       .byte $00 ; |        | $F1B7
Zero
       .byte $00 ; |        | $F1B8
       .byte $7C ; | XXXXX  | $F1B9
       .byte $C6 ; |XX   XX | $F1BA
       .byte $E6 ; |XXX  XX | $F1BB
       .byte $D6 ; |XX X XX | $F1BC
       .byte $CE ; |XX  XXX | $F1BD
       .byte $C6 ; |XX   XX | $F1BE
       .byte $7C ; | XXXXX  | $F1BF
One
       .byte $00 ; |        | $F1C0
       .byte $FC ; |XXXXXX  | $F1C1
       .byte $30 ; |  XX    | $F1C2
       .byte $30 ; |  XX    | $F1C3
       .byte $30 ; |  XX    | $F1C4
       .byte $30 ; |  XX    | $F1C5
       .byte $70 ; | XXX    | $F1C6
       .byte $30 ; |  XX    | $F1C7
Two
       .byte $00 ; |        | $F1C8
       .byte $FE ; |XXXXXXX | $F1C9
       .byte $E0 ; |XXX     | $F1CA
       .byte $78 ; | XXXX   | $F1CB
       .byte $3C ; |  XXXX  | $F1CC
       .byte $0E ; |    XXX | $F1CD
       .byte $C6 ; |XX   XX | $F1CE
       .byte $7C ; | XXXXX  | $F1CF
Three
       .byte $00 ; |        | $F1D0
       .byte $7C ; | XXXXX  | $F1D1
       .byte $C6 ; |XX   XX | $F1D2
       .byte $06 ; |     XX | $F1D3
       .byte $3C ; |  XXXX  | $F1D4
       .byte $18 ; |   XX   | $F1D5
       .byte $0C ; |    XX  | $F1D6
       .byte $7E ; | XXXXXX | $F1D7
Four
       .byte $00 ; |        | $F1D8
       .byte $0C ; |    XX  | $F1D9
       .byte $0C ; |    XX  | $F1DA
       .byte $FE ; |XXXXXXX | $F1DB
       .byte $CC ; |XX  XX  | $F1DC
       .byte $6C ; | XX XX  | $F1DD
       .byte $3C ; |  XXXX  | $F1DE
       .byte $1C ; |   XXX  | $F1DF

       align 256
       .byte 0		;		so following data is offset by one byte
CharSetPageTwo
Five
       .byte $00 ; |        | $F1E0
       .byte $7C ; | XXXXX  | $F1E1
       .byte $C6 ; |XX   XX | $F1E2
       .byte $06 ; |     XX | $F1E3
       .byte $06 ; |     XX | $F1E4
       .byte $FC ; |XXXXXX  | $F1E5
       .byte $C0 ; |XX      | $F1E6
       .byte $FC ; |XXXXXX  | $F1E7
Six
       .byte $00 ; |        | $F1E8
       .byte $7C ; | XXXXX  | $F1E9
       .byte $C6 ; |XX   XX | $F1EA
       .byte $C6 ; |XX   XX | $F1EB
       .byte $FC ; |XXXXXX  | $F1EC
       .byte $C0 ; |XX      | $F1ED
       .byte $60 ; | XX     | $F1EE
       .byte $3C ; |  XXXX  | $F1EF
Seven
       .byte $00 ; |        | $F1F0
       .byte $30 ; |  XX    | $F1F1
       .byte $30 ; |  XX    | $F1F2
       .byte $30 ; |  XX    | $F1F3
       .byte $18 ; |   XX   | $F1F4
       .byte $0C ; |    XX  | $F1F5
       .byte $C6 ; |XX   XX | $F1F6
       .byte $FE ; |XXXXXXX | $F1F7
Eight
       .byte $00 ; |        | $F1F8
       .byte $7C ; | XXXXX  | $F1F9
       .byte $C6 ; |XX   XX | $F1FA
       .byte $C6 ; |XX   XX | $F1FB
       .byte $7C ; | XXXXX  | $F1FC
       .byte $C6 ; |XX   XX | $F1FD
       .byte $C6 ; |XX   XX | $F1FE
       .byte $7C ; | XXXXX  | $F1FF
Nine
       .byte $00 ; |        | $F200
       .byte $78 ; | XXXX   | $F201
       .byte $0C ; |    XX  | $F202
       .byte $06 ; |     XX | $F203
       .byte $7E ; | XXXXXX | $F204
       .byte $C6 ; |XX   XX | $F205
       .byte $C6 ; |XX   XX | $F206
       .byte $7C ; | XXXXX  | $F207
Multiply
       .byte $00 ; |        | $F208
       .byte $00 ; |        | $F209
       .byte $44 ; | X   X  | $F20A
       .byte $28 ; |  X X   | $F20B
       .byte $10 ; |   X    | $F20C
       .byte $28 ; |  X X   | $F20D
       .byte $44 ; | X   X  | $F20E
       .byte $00 ; |        | $F20F
Divide
       .byte $00 ; |        | $F210
       .byte $00 ; |        | $F211
       .byte $10 ; |   X    | $F212
       .byte $00 ; |        | $F213
       .byte $FE ; |XXXXXXX | $F214
       .byte $00 ; |        | $F215
       .byte $10 ; |   X    | $F216
       .byte $00 ; |        | $F217
Add
       .byte $00 ; |        | $F218
       .byte $10 ; |   X    | $F219
       .byte $10 ; |   X    | $F21A
       .byte $10 ; |   X    | $F21B
       .byte $FE ; |XXXXXXX | $F21C
       .byte $10 ; |   X    | $F21D
       .byte $10 ; |   X    | $F21E
       .byte $10 ; |   X    | $F21F
Subtract
       .byte $00 ; |        | $F220
       .byte $00 ; |        | $F221
       .byte $00 ; |        | $F222
       .byte $00 ; |        | $F223
       .byte $FE ; |XXXXXXX | $F224
       .byte $00 ; |        | $F225
       .byte $00 ; |        | $F226
       .byte $00 ; |        | $F227
Equal
       .byte $00 ; |        | $F228
       .byte $00 ; |        | $F229
       .byte $00 ; |        | $F22A
       .byte $FE ; |XXXXXXX | $F22B
       .byte $00 ; |        | $F22C
       .byte $FE ; |XXXXXXX | $F22D
       .byte $00 ; |        | $F22E
       .byte $00 ; |        | $F22F
GreaterThan
       .byte $00 ; |        | $F230
       .byte $20 ; |  X     | $F231
       .byte $10 ; |   X    | $F232
       .byte $08 ; |    X   | $F233
       .byte $04 ; |     X  | $F234
       .byte $08 ; |    X   | $F235
       .byte $10 ; |   X    | $F236
       .byte $20 ; |  X     | $F237
LessThan
       .byte $00 ; |        | $F238
       .byte $08 ; |    X   | $F239
       .byte $10 ; |   X    | $F23A
       .byte $20 ; |  X     | $F23B
       .byte $40 ; | X      | $F23C
       .byte $20 ; |  X     | $F23D
       .byte $10 ; |   X    | $F23E
       .byte $08 ; |    X   | $F23F
LeftArrow
       .byte $00 ; |        | $F240
       .byte $00 ; |        | $F241
       .byte $20 ; |  X     | $F242
       .byte $40 ; | X      | $F243
       .byte $FE ; |XXXXXXX | $F244
       .byte $40 ; | X      | $F245
       .byte $20 ; |  X     | $F246
       .byte $00 ; |        | $F247
Comma
        .byte #%00000000
        .byte #%01100000
        .byte #%00110000
        .byte #%00110000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
Period
        .byte #%00000000
        .byte #%01100000
        .byte #%01100000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
RightParens
       .byte $00 ; |        | $F258
       .byte $20 ; |  X     | $F259
       .byte $10 ; |   X    | $F25A
       .byte $08 ; |    X   | $F25B
       .byte $08 ; |    X   | $F25C
       .byte $08 ; |    X   | $F25D
       .byte $10 ; |   X    | $F25E
       .byte $20 ; |  X     | $F25F
LeftParens
       .byte $00 ; |        | $F260
       .byte $08 ; |    X   | $F261
       .byte $10 ; |   X    | $F262
       .byte $20 ; |  X     | $F263
       .byte $20 ; |  X     | $F264
       .byte $20 ; |  X     | $F265
       .byte $10 ; |   X    | $F266
       .byte $08 ; |    X   | $F267
       .byte $00 ; |        | $F268

TextBlock
       .byte T,H,I,S,_,I,S,_,T,H,E,_
       .byte E,N,T,I,R,E,_,_,_,_,_,_
       .byte C,H,A,R,A,C,T,E,R,_,_,_ 
       .byte S,E,T,Period,_,_,_,_,_,_,_,_
       .byte A,B,C,D,E,F,G,H,I,J,K,L
       .byte M,N,O,P,Q,R,S,T,U,V,W,XX
       .byte YY,Z,Zero,One,Two,Three,Four,Five,Six,Seven,Eight,Nine
       .byte Multiply,Divide,Add,Subtract,Equal,LessThan,GreaterThan,LeftArrow,Comma,Period,LeftParens,RightParens
       .byte 255
;-------------------------------------------------------------------------
;-------------------------End Data----------------------------------------
;-------------------------------------------------------------------------
       ORG $FFFC
       .word Start
       .word Start
