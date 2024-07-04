;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Brawdy2k
;  Game programed by R. Scharmen
;  March 2024 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    processor 6502

    include "vcs.h"
    include "macro.h"
    include "xmacro.h"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start an uninitialized segment at $80 for var declaration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_Variables:

Player0y		.byte
Player0x		.byte
Player1y		.byte
Player1x		.byte
EnemyXLoc       .byte
Random          .byte
rand16          .word
EnemySpritePtr  .word
EnemyColorPtr   .word
P0SpritePtr     .word
P0ColorPtr      .Word
DamageClrPtr    .word
DestroyAnim     .byte
AnimCounter     .byte
EnemyAnimOffset .word
Missile0x       .byte
Missile0y       .byte
OnesDigitOffset .word        
TensDigitOffset .word         
Score           .byte        
HP              .byte         
Temp            .byte         
ScoreSprite     .byte        
TimerSprite     .byte         
Gameover        .byte
BackgroundClr   .byte
EnemyCount      .byte
P0AnimOffset    .word
EnemyDropLevel  .byte
Level           .byte
Center          .byte
CenterLevel     .byte
Exp             .byte
AddHP           .byte
EFFECT1         .byte
SoundOff        .byte
MUSIC1          .byte
EFFECT2         .byte
ExpOff          .byte
Pause           .byte
Stop            .byte
StopMove        .byte
Delay           .byte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0SPRITEHGT	= 8
P1SPRITEHGT	= 8
DIGITS_HEIGHT = 5 
    lda #1
    sta Pause    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC FIRE_MISSILE     
    lda #%00000000
    bne SkipMissile
    cpx Missile0y
    bne SkipMissile
    lda #%00000010   
    inc Missile0y      
    inc Missile0y
SkipMissile:
    sta ENAM0
    ENDM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code segment for 2k game at $F800
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    seg Code
    org $F800
Reset:
    CLEAN_START
    lda #$CA
    sta BackgroundClr 
    jsr GetRandomPos
    jsr GetRandomCenter    
    jsr GetRandomLevel
    jmp SetVar   
Unpause
    lda #$0A
    sta BackgroundClr      
    beq Unpause
    lda #1
    sta Pause
    lda #82
    sta Player1x
SetVar:
    sta Player1x
    lda #1
    sta Player0y    
    lda #82
    sta Player0x
    lda #95
    sta Player1y
    lda #212
    sta Random 
    lda #20
    sta Level 
    sta Center
    lda #0
    sta EnemyAnimOffset
    sta GameOver
    sta P0AnimOffset
    sta AddHP
    sta EnemyCount
    sta Score
    sta SoundOff
    sta ExpOff 
    lda #50
    sta AnimCounter
    lda #16
    sta DestroyAnim 
    lda #$25
    sta HP  
    lda #20
    sta Stop
    lda #5
    sta Delay          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set Pointer Variaibles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Pointers:
    lda #<P1Frame0
    sta EnemySpritePtr      
    lda #>P1Frame0
    sta EnemySpritePtr+1   

    lda #<P1ColorFrame0
    sta EnemyColorPtr      
    lda #>P1ColorFrame0
    sta EnemyColorPtr+1

    lda #<P0Frame0
    sta P0SpritePtr      
    lda #>P0Frame0
    sta P0SpritePtr+1   

    lda #<P0ColorFrame0
    sta P0ColorPtr      
    lda #>P0ColorFrame0
    sta P0ColorPtr+1

    lda #<DateColorPF0
    sta DamageClrPtr
    lda #>DateColorPF0
    sta DamageClrPtr+1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Start:   
    lda #2
    sta VBLANK     
    sta VSYNC      
    lsr SWCHB    
    bcc Unpause    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_VSYNC:
    REPEAT 3
    sta WSYNC  
    REPEND
    lda #0
    sta VSYNC  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the 37 recommended lines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    TIMER_SETUP 37
ExpSound:
    lda ExpOff 
    cmp #1
    bcs ExpSoundOff
    LDA EFFECT2	
	BEQ ExpSoundOff		
MakeExpSound:	
	LDA EFFECT2	
	BEQ ExpSoundOff		
	DEC EFFECT2	
	LSR		
	LSR		
	STA AUDV1	
    LSR		
	EOR #7			
	ADC #1	
	STA AUDF1
	LDA #8	
	STA AUDC1
	JMP GameSettings
ExpSoundOff:	
    LDA #0
    sta ExpOff
    LDA #0
	STA AUDV1
GameSettings:   
    lda #$F0
    sta COLUPF 
NewStart1 
    lda Pause
    cmp #0
    beq Resume1
    jsr Joystick
Resume1
    clc
    lda AnimCounter
    adc #1
    sta AnimCounter
    cmp #8
    bcc .SkipAnim
    lda #0
    sta AnimCounter
    sta P0AnimOffset          
    lda Score
    cmp #5
    beq .AddHP
    cmp #37
    beq .AddHP
    cmp #80
    beq .AddHP
    lda Score
    cmp #117
    beq .AddHP     
    jmp EnemyAnim
.AddHP
    lda #1
    inc AddHP
    SED
    lda HP
    clc
    adc #5 
    sta HP
    lda Score
    adc #1
    sta Score
    cld
EnemyAnim:
    lda EnemyAnimOffset
    cmp DestroyAnim
    bcc .Enemyadd    
    lda #0
    sta EnemyAnimOffset
    jmp .SkipAnim
.Enemyadd:
    clc    
    lda EnemyAnimOffset
    adc #9
    sta EnemyAnimOffset
.SkipAnim:
    lda GameOver
    cmp #0
    beq .GameoverScreen
	lda Player0x
    ldx #0		
    jsr SetHorizPos
    lda Player1x	
    ldx #1		
    jsr SetHorizPos
    lda Missile0x
    ldx #2
    jsr SetHorizPos
.GameoverScreen
    jsr CalculateDigitOffset
    sta WSYNC
    sta HMOVE
    sta WSYNC    

    TIMER_WAIT
    lda #0
    sta VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Scoreboard:
    lda #0                
    sta COLUBK
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    lda #%00000010
    sta CTRLPF
    lda #$1E
    sta COLUP0
    lda #$0E
    sta COLUP1    
    sta WSYNC
    lda #$1E
    sta COLUPF    
    lda HP
    cmp #11  
    bcs .ScoreCOLUBK
    lda #$34
    sta COLUP1    
.ScoreCOLUBK    
    lda #$72
    sta COLUBK     
    ldx #DIGITS_HEIGHT 
.ScoreDigitLoop:
    ldy TensDigitOffset      ; get the tens digit offset for the Score
    lda Digits,Y             ; load the bit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta ScoreSprite          ; save the score tens digit pattern in a variable
    ldy OnesDigitOffset      ; get the ones digit offset for the Score
    lda Digits,Y             ; load the digit bit pattern from lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora ScoreSprite          ; merge it with the saved tens digit sprite
    sta ScoreSprite          ; and save it
    sta WSYNC                ; wait for the end of scanline
    sta PF1                  ; update the playfield to display the Score sprite
    ldy TensDigitOffset+1    ; get the left digit offset for the HitPoints
    lda Digits,Y             ; load the digit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta TimerSprite          ; save the Wavetens digit pattern in a variable
    ldy OnesDigitOffset+1    ; get the ones digit offset for the HitPoints
    lda Digits,Y             ; load digit pattern from the lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora TimerSprite          ; merge with the saved tens digit graphics
    sta TimerSprite         ; and save it
    jsr Sleep12Cycles        ; wastes some cycles
    sta PF1                  ; update the playfield for Wavedisplay
    ldy ScoreSprite          ; preload for the next scanline
    sta WSYNC                ; wait for next scanline
    sty PF1                ; update playfield for the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1    ; increment all digits for the next line of data
    jsr Sleep12Cycles        ; waste some cycles
    dex                     
    sta PF1                  
    bne .ScoreDigitLoop
    sta WSYNC
SetScreen:
    lda #0
    sta PF0
    sta PF1
    sta PF2    
    sta WSYNC
    sta WSYNC    
    lda #0     
    sta ENAM0
    lda #50
    sta EnemyXLoc 
    lda BackgroundClr
    sta COLUBK
    lda #1
    sta CTRLPF
FireSoundCheck:   
    lda SoundOff
    cmp #1
    bcs NoSound
    LDA EFFECT1	; Read effect 1 counter.
    BEQ NoSound		; Jump if zero.
	DEC EFFECT1	; Count towards zero.
	CLC
	ADC #10	; Volume based on counter (12-2)
	STA AUDV0
	LSR
    STA AUDF0
	LDA #1		
	STA AUDC0    
	JMP EndSoundEffects
NoSound	
    LDA #0
	STA AUDV0	; Turn off sound.
    LDA  #0
    sta SoundOff
EndSoundEffects:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Kernal 2 Scan Lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
GameScreen   
    ldx # 88
PlayField:    
    txa     
    tay
    lda (DamageClrPtr),y 
    sta COLUPF 
    lda DataPF2,y
    sta PF2
EnemySprite
.Sprite1Scan    
	txa		; X -> A
    sec		; set carry for subtract
    sbc Player1y	; local coordinate
    cmp #P1SPRITEHGT ; in sprite?
    bcc .InSprite1	; yes, skip over next
    lda #0		; not in sprite, load 0
.InSprite1
    clc
    adc EnemyAnimOffset
	tay		; local coord -> Y
    lda (EnemySpritePtr),Y	; lookup color
    sta WSYNC	; sync w/ scanline
    sta GRP1	; store bitmap
    lda (EnemyColorPtr),Y ; lookup color
    sta COLUP1	; store color 
FireMissileMacro    
   FIRE_MISSILE
P0Sprite 
.Sprite0Scan
	txa		; X -> A
    sec		; set carry for subtract
    sbc Player0y	; local coordinate
    cmp #P0SPRITEHGT ; in sprite?
    bcc .InSprite0	; yes, skip over next
    lda #0		; not in sprite, load 0
.InSprite0 
    clc
    adc P0AnimOffset
	tay		; local coord -> Y
    lda (P0SpritePtr),y	; lookup color
    sta WSYNC	; sync w/ scanline
    sta GRP0	; store bitmap
    lda (P0ColorPtr),y ; lookup color
    sta COLUP0	; store color
    dex		; decrement X
    bne PlayField	;    
NewStart2
    lda Pause
    cmp #0
    beq _Resume1
    jsr EnemyDrop
_Resume1    
    lda #$00
    sta COLUBK    
    sta PF0
    sta PF1
    sta PF2    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Collisions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
Combat:
.P1PFCollision:
    lda #%10000000          
    bit CXP1FB              
    bne .CollisionPF  
.P1P0Collision:
    lda #%10000000
    bit CXPPMM 
    bne .CollisionP0
.MissileCol:
    lda #%10000000
    bit CXM0P
    bne .EnemyHit
    jmp .SetFrame
.CollisionPF
    lda #1
    sta ExpOff
    LDA #50	; Counter for effect 1.
	STA EFFECT2
    jsr GetRandomLevel    
    lda EnemyCount
    clc
    adc #1
    sta EnemyCount   
    jsr GetRandomPos
    sta Player1x
    ldx #100
    stx Player1y 
    sed
    lda HP
    sec
    sbc #2
    sta HP
    cld
    cmp #1
    bpl .EndCollisionCheck  
    lda #0
    sta Pause    
    jmp GameOver
.CollisionP0
    lda #1
    sta ExpOff
    LDA #50	; Counter for effect 1.
	STA EFFECT2
    jsr GetRandomLevel    
    lda EnemyCount
    clc
    adc #1
    sta EnemyCount    
    jsr GetRandomPos
    sta Player1x
    ldx #80
    stx Player1y 
    sed
    lda HP
    sec
    sbc #1
    sta HP       
    cld
    cmp #1
    bpl .EndCollisionCheck
    lda #0
    sta Pause
    jmp GameOver
.EnemyHit:
    lda #1
    sta ExpOff
    LDA #50	; Counter for effect 1.
	STA EFFECT2    
    jsr GetRandomLevel    
    lda EnemyCount
    clc
    adc #1
    sta EnemyCount
    sed
    lda Score
    clc
    adc #1
    sta Score
    cld
    lda #0
    sta Missile0y
    sta ENAM0 
    jsr GetRandomPos
    sta Player1x
    ldx #80
    stx Player1y
    jmp .EndCollisionCheck
.SetFrame
    sta WSYNC
    sta WSYNC
        
.EndCollisionCheck:           
    sta CXCLR
Vblank:
    sta WSYNC
    lda #$B2
    sta COLUBK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 27 more VBLANK overscan lines to complete our frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 27
    sta WSYNC          
    REPEND   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop to next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EnemyDrop subroutine 
    ldx Player1y      
    cpx EnemyDropLevel
    beq .Move 
.Drop: 
    lda #%00000101
    sta NUSIZ1     
    ldx Player1y
    cpx #2
    bcc .Move    
    dex
    lda EnemyCount
    cmp #30
    bcc .Slow
    lda #%00100000
    sta NUSIZ0    
    lda EnemyCount
    cmp #60
    bcc .Slow 
    lda #%00000000
    sta NUSIZ1  
    lda EnemyCount
    cmp #90
    bcc .Slow 
    lda #%00000000
    sta NUSIZ0       
.Slow
    sta WSYNC
    stx Player1y  
.SkipDrop     
    rts
Movement:
.Move    
    lda Player1x
    cmp #82
    beq .Drop
    cmp #82
    bcs .MoveLeft
.MoveRight    
    ldx Player1y        
    cpx #EnemyDropLevel   
    bcs .Right          
    stx Player1y
.Right
    ldx Player1x
    cpx #CenterLevel
    bcs .Drop    
    ldx Player1x
    cpx #150
    bcs OffScreen   
    inx 
    stx Player1x    
    rts
.MoveLeft:
    ldx Player1y      
    cpx #CenterLevel
    bcc .Left
    ldx #2      
    stx Player1y
.Left      
    ldx Player1x
    cmp #CenterLevel
    bcs .Drop
    ldx Player1x
    cpx #18
    bcc OffScreen
    dex 
    stx Player1x
.SkipAllMoves
    rts
OffScreen subroutine
    lda #250
    sta Player1x
    sta Player1y    
    jsr GetRandomPos
    sta Player1x
    ldx #80
    stx Player1y 
    lda #55 
    sta EnemyDropLevel    	  
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Horizontial Movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetHorizPos subroutine
	sta WSYNC	
	sec	
.DivideLoop:
	sbc #15
	bcs .DivideLoop	
	eor #7		
	asl
	asl
	asl
	asl
	sta RESP0,x	
	sta HMP0,x	
	rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Joystick Controls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Joystick subroutine
    lda Missile0y
    cmp #90
    bcc .MissileinAir
    lda #0
    sta Missile0y
.MissileinAir
    ldx Player0x
    lda Missile0y
    cmp #6
    bcs .MoveJoystick    
.FireButton: 
    lda #1
    sta SoundOff
    lda #15		
	STA EFFECT1      
    ldx Player0x
    lda #%10000000    
    bit INPT4
    bne .MoveJoystick    
    lda Player0x
    clc
    adc #2
    sta Missile0x
    lda Player0y
    clc
    adc #5
    sta Missile0y
    ldy #%00110000
    sty NUSIZ0    
.MoveJoystick:    
	lda #%01000000	;Left?
	bit SWCHA
	bne .SkipMoveLeft
    cpx #16
    bcc .SkipMoveLeft
    ldy #%00110000    
    dex
    dex
    lda #9    
    sta P0AnimOffset    
.SkipMoveLeft:
	lda #%10000000	;Right?
	bit SWCHA 
	bne .SkipMoveRight
    cpx #153
    bcs .SkipMoveRight
    ldy #%00110000  
    inx
    inx
    lda #18    
    sta P0AnimOffset 
.SkipMoveRight:    
	stx Player0x    
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Random Position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random               
    lsr    
    sta EnemyXLoc           
    lda #15
    adc EnemyXLoc           
    sta EnemyXLoc                
    rts
GetRandomLevel subroutine
    lda Level
    asl
    eor Level
    asl
    eor Level
    asl
    asl
    eor Level
    asl
    rol Level              
    lsr    
    sta EnemyDropLevel           
    lda #5
    adc EnemyDropLevel           
    sta EnemyDropLevel                
    rts
GetRandomCenter subroutine
    lda Center
    asl
    eor Center
    asl
    eor Center
    asl
    asl
    eor Center
    asl
    rol Center             
    lsr    
    sta CenterLevel           
    lda #60
    adc CenterLevel           
    sta CenterLevel                
    rts
CalculateDigitOffset subroutine
    ldx #1    
ScoreLoop subroutine
    lda Score,X             
    and #$0F                 
    sta Temp               
    asl                      
    asl                      
    adc Temp               
    sta OnesDigitOffset,X  
    lda Score,X            
    and #$F0               
    lsr                   
    lsr                    
    sta Temp             
    lsr                    
    lsr   
    adc Temp                 
    sta TensDigitOffset,X  
    dex                      
    bpl ScoreLoop    
    rts
Sleep12Cycles subroutine
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Gameover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #0
    sta Pause
    sta HP
    lda #$30
    sta BackgroundClr    
    lda #%0000001
    sta NUSIZ1
    lda #77
    sta Player1x
    lda #10
    sta Player1y
    lda #200
    sta Player0y
    REPEAT 28   
    sta WSYNC
    REPEND
    jmp Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Graphics Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Player0:
P0Frame0
        .byte #%00000000
        .byte #%00100100;$80
        .byte #%00111100;$80
        .byte #%00011000;$80
        .byte #%00100100;$32
        .byte #%00100100;$32
        .byte #%00111100;$32
        .byte #%01011010;$70
        .byte #%00000000;--
P0Frame1
        .byte #%00000000
        .byte #%00100100;$80
        .byte #%00111100;$80
        .byte #%10011000;$80
        .byte #%10100100;$32
        .byte #%11100100;$32
        .byte #%10111100;$32
        .byte #%10111010;$70
        .byte #%00000000;--
P0Frame2
        .byte #%00000000
        .byte #%00100100;$80
        .byte #%00111100;$80
        .byte #%00011001;$80
        .byte #%00100101;$32
        .byte #%00100111;$32
        .byte #%00111101;$32
        .byte #%01011101;$70
        .byte #%00000000;--
;---End Graphics Data---
P0ColorFrame0
        .byte #%00000000
        .byte #$80;
        .byte #$80;
        .byte #$80;
        .byte #$80;
        .byte #$80;
        .byte #$80;
        .byte #$70;
        .byte #$0E;
P0ColorFrame1
        .byte #%00000000
        .byte #$80;
        .byte #$80;
        .byte #$80;
        .byte #$32;
        .byte #$32;
        .byte #$32;
        .byte #$70;
        .byte #$0E;
P0ColorFrame2
        .byte #%00000000
        .byte #$80;
        .byte #$80;
        .byte #$80;
        .byte #$32;
        .byte #$32;
        .byte #$32;
        .byte #$70;
        .byte #$0E;
P0ColorFrame4
        .byte #%00000000
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
;---End Color Data---

P1Frame0
    .byte #%00000000
    .byte #%10000001
    .byte #%01000010
    .byte #%01011010
    .byte #%11011011
    .byte #%00100100
    .byte #%10011001
    .byte #%01011010
    .byte #%00100100
P1Frame1
    .byte #%00000000
    .byte #%01000010
    .byte #%01000010
    .byte #%01011010
    .byte #%01011010
    .byte #%00100100
    .byte #%00011000
    .byte #%11011011
    .byte #%00100100
P1Frame2
    .byte #%00000000
    .byte #%00100100
    .byte #%01000010
    .byte #%01011010
    .byte #%11011011
    .byte #%00100100
    .byte #%10011001
    .byte #%01011010
    .byte #%00100100    
P1ColorFrame0
    .byte #%00000000
    .byte $30;
    .byte $44;
    .byte $B2;
    .byte $C4;
    .byte $42;
    .byte $C0;
    .byte $30;
    .byte $0E;
P1ColorFrame1
    .byte #%00000000
    .byte $30;
    .byte $44;
    .byte $B2;
    .byte $C4;
    .byte $42;
    .byte $C0;
    .byte $30;
    .byte $0E;
P1ColorFrame2
    .byte #%00000000
    .byte $30;
    .byte $44;
    .byte $B2;
    .byte $C4;
    .byte $42;
    .byte $C0;
    .byte $30;
    .byte $0E;
;---End Color Data---

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PlayField Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DataPF2:    
	.byte $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
	.byte $8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C
	.byte $8C,$8C,$8C,$FC,$FC,$FC,$FE,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00	
DateColorPF0    
    .byte $00
    .byte $00
    .byte $10
    .byte $12
    .byte $10
    .byte $12
    .byte $10
    .byte $12
    .byte $10
    .byte $12
    .byte $10
    .byte $12
    .byte $10
    .byte $12
    .byte $10
    .byte $12
    .byte $10
    .byte $12
    .byte $10
    .byte $12
    .byte $10
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Digit Graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ECHO ([$FF00-*]d), "bytes free"
    org $FFFC
    .word Reset
    .word Reset