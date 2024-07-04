;--------------------------------------------------------
;--------------------------------------------------------
;----The Abstract Assembly Development Kit:            --
;----an Assembly Language Programming Framework        --
;----For the Atari 2600 VCS                            --
;--------------------------------------------------------
;---- Brought to you by:                               --
;---- Mr SQL, RELATIONALFRAMEWORK.COM                  --
;--------------------------------------------------------
;----Anyone may use this kit however they wish         --
;----No strings attached.                              --
;--------------------------------------------------------
;--------------------------------------------------------
;----                                                  --
;----Source is fully commented and the doc's also      --
;----explain the abstract calls to the                 --
;----phantom hardware for rendering                    --
;----large scrolling WYSIWYG bitmaps, and setting      --
;----or checking the bits, panning the camera & scaling-- 
;--------------------------------------------------------
    processor 6502    ;                                --
    include "vcs.h"   ;                                --
;;    include "macro.h" ;                                --
;-------------------------------------------------------- 
	
	seg.u RIOT_vars
	org $80
	
	
	seg.u FA_RAM_vars
	org $1000


;------------------------------------------------------------------------------
;--- RAM VARIABLE MAP
;------------------------------------------------------------------------------
; SuperChip Reminder: if you want to write:
; use $F000-$F07F, but if you want to read, use $F080-$F0FF.
; now using the CBS RAM superchip for 256 extra bytes of RAM
; to renders the large 240 byte virtual worlds dynamic
;------------------------------------------------------------------------------

                                                           
;DYNAMICSYNC = $8c
o=$8c

;--- two bytes for music engine, pointer & sustain (score is ROM data block)
MUSICINDEX         = $80                  ; storage location (1st byte in RAM)
SUSTAINFORFRAMES   = $81 ; 129

a = $82 ; 10 bytes for variables, a-j
b = $83 ; note: vars a,b,c,d,e are temp vars so use the stack if you need to preserve them!
c = $84
d = $85
e = $86 ; music engine is using this (it shouldn't) so also a temp var, push to preserve
f = $87
g = $88  
h = $89  
i = $8a  
j = $8b  
;REVBT = $8d
; REVBT var and ReverseB (Reversebyte) routine are not used at all.
YIndex = $8d ; reusing this location then, will be used to calculate BYTErowoffset as a large virtual world Y value for concept (not necessary)

PLAYFIELDINDEX = $8e
PLAYFIELDINDEXstep = $8f ; 18 bytes for variables so far...

;reserving another 15 bytes, $90-9F
; --- virtual world variables
BITIndex = $90
BYTErowoffset = $9d ; incs 1 y position in virtual world for each rowsize (12 bytes);goes  with BITIndex (x position)

p0 = $91
p1 = $92
p2 = $93

;-- Scrolling Demo Vars
scrollspeed = $94 ; scrollspeed!
scrollspeedinit = $95; scrollspeed fade... changed to scroll direction (not so intuitive)
;-- End Scrolling Demo vars
;--------------------------- SCROLLOUT Game Vars
px = $96        ; used for game status ( no player x!)
py = $97; player y (there is no player X in Scrollout, it's always on the left)

bx = $98; ball x
by = $99; ball y

bitx = $9a 
bity = $9b
bitstatus = $9c
; $9d in use for BYTErowoffset
player0x = $9e
player0y = $9f

;--------------------------- End Scrollout Game Vars


RAMplayfield = $A0 ; - BF$
; using $A0-DF$ (60 bytes) to hold playfield image (20x10 in 40x10 grid)
; Note: half of this screen buffer, 30 bytes, is loaded WYSIWYG from the virtual world buffer (240 bytes),
; then it is expanded 2:1 to 60 bytes and flipped to and fro for the display.

player1x = $e0
player1y = $e1 ; 95 bytes of RAM reserved so far; 35 + 60 for the screen buffer

missile0x = $e2
missile0y = $e3

missile1x = $e4
missile1y = $e5

;ball0x = $e6 
;ball0y = $e7
p = $e6 
q = $e7

k = $e8
l = $e9
m = $ea
n = $eb ; now 105 bytes of system RAM reserved; 23 remaining
o = $ec ; 22 remaining... must leave most of that for the stack ;)
;------------------------------------------------------------------------------
 ; now using CBS RAM for 2x the SC .. 256 bytes!
;SuperChipWRITE = $F000
MyAbstractExtendedPlayfieldSCW = $F000; $1000   ; $F000
;SuperChipREAD = $F080
MyAbstractExtendedPlayfieldSCR = $F100 ; 256 bytes now CBS RAM!   $F080; $1080  ; $F080
 ; makes no diff offset by 1 for cbs ram overwrite bug
 ; find write bug... there was no write bug
 ; ---- end ram vars

 ; TOP 16 bytes of high RAM (top of CBS RAM) to load sprite data
Sprite0SCW = ($F000 + $F0) ; + 240
Sprite0SCR = ($F100 + $F0) ; + 240

;Sprite1SCW = ($F000 + 248) ; + $F8
Sprite1SCW = ($F000 + $f8) ; + $F8
Sprite1SCR = ($F100 + $f8)


; The phantom hardware engines and your game code live in the first 3.5k bank (plenty of space)
; "All large virtual world bitmaps live in the 2nd 3.5k bank along with the music engine and it's data
; 3rd 3.5k bank is free but you shouldn't need it


	seg bank_0
	org $D000
	rorg $F000
	
	repeat 512
	.byte $D0
	repend
Bank0
        nop
        nop
        nop
; these calls at the top of ROM are proxy/stub in both banks:
; the lines of code alternately don't execute (stub) but have a
; stub that does execute in the alternate bank -
; more simply, "Same matter can't occupy same space."

 jmp jmparoundcallbank1ForMusicEngine
callbank1ForMusicEngine
 lda $fff9 ; transfer to the same spot in bank1
 jsr callbank1ForMusicEngine ; dummy call placeholder, never executes (not recursive, will not loop and flood the stack)
 lda $1000 ; dummy placeholder for return call (lda $fff8/bank 0), never executes
 rts ; executes

CallBank1ToLoadCBSRAMwithLargeBitmap
 lda $fff9; transfer to the same spot in bank1
 jsr callbank1ForMusicEngine ; dummy call placeholder, never executes (not recursive, will not loop and flood the stack)
 lda $1000 ; dummy placeholder for return call (lda $fff8/bank 0), never executes
 rts ; executes

CallBank1ToLoadCBSRAMwithLargeBitmapSayingREADYPLAYER1
 lda $fff9; transfer to the same spot in bank1
 jsr callbank1ForMusicEngine ; dummy call placeholder, never executes (not recursive, will not loop and flood the stack)
 lda $1000 ; dummy placeholder for return call (lda $fff8/bank 0), never executes
 rts ; executes

CallBank1ToLoadCBSRAMwithGameScreenBitmap
 lda $fff9; transfer to the same spot in bank1
 jsr callbank1ForMusicEngine ; dummy call placeholder, never executes (not recursive, will not loop and flood the stack)
 lda $1000 ; dummy placeholder for return call (lda $fff8/bank 0), never executes
 rts ; executes


jmparoundcallbank1ForMusicEngine


CLEAN_START  ; ----SCROLLOUT ; LARGE PLAY AREA IS MALLEABLE IN CBS RAM
                     ; ---- see routine in bank 1 (Note: with CBS RAM only 3.5k in bank0; don't want to waste 1/4 k on each large ROM WYSIWYG bitmap here or we will quickly run out of space for game code!)
;---- SCROLLOUT CORE is in FrameRelay (entire frame for plenty of cycles) along with the Twin Engines for abstract rendering
;;Reset


 ;clear ramplayfield check:
 ldx #30 ; clear 1/2 the ram page
 lda #0

clearram sta RAMplayfield,x
 dex
 bne clearram

 ;------------------------
 ;----------------------------------------
; ... ok init registers:
 ldx #128
regs sta 0,x
 dex
 bne regs
; just the stack:
   ldx #255
   txs ;init stack to FF - memory on the 2600 is $80-$FF!

  lda #22
  sta scrollspeed ; set speed for demo
  lda #0 ; test 
  sta scrollspeedinit
  sta BITIndex ; column 5 breaking?

  ;lda #36 ; test 3rd row down start (0,3)
    lda #0
   sta  BYTErowoffset ; each row size (12 bytes) inc's Y value 1 from (x,0)

    lda #0
    sta SUSTAINFORFRAMES; intialize music engine to read score
    sta PLAYFIELDINDEX; init playfield index offset
    
  ; lda #0
    sta MUSICINDEX ; initialize music engine ROM song offset at beginning of score

    ;sta SWACNT ; ($0281), configure all 8 bits of SWCHA ($0280) for input
    ; should already be set

    lda #10
    sta PLAYFIELDINDEXstep ; count down to 0 and reset for each inc of index

                lda #50
                sta COLUPF ; playfield colour

                lda #%00100101
;------------------------------------
; init section:                    --
;------------------------------------

  ;-- init variable F for sprite animation demo:
  lda #24
  sta f

  ;-- Put player sprites in center of screen
  lda #45 ; (Y value runs from 1-96 so 45 is center)
  sta player0y ; non zero coordinate values also initialises sprite to appear
  sta player1y
  lda #70 ; (x value runs from 1 to 150 so 70 and 80 are next to center)
  sta player0x
  lda #80
  sta player1x 

  ;-- Hide Missile sprites (0 the x and y coordinate variables)
  lda #0
  sta missile0x
  sta missile0y
  sta missile1x
  sta missile1y

  lda #$58
  sta COLUP0 ; colour of player0 (player 1)
 lda #100
 sta COLUP1


 lda #$c4 ;#$b6;#75
 sta COLUPF ; playfield solid colour


 ;-- load default sprite image 1 into player 0 sprite:
 ldy #0
 jsr loadplayer0

 ;-- load default sprite image 2 for player 1 sprite:
 ldy #8
 jsr loadplayer1


; ----- subroutine: load large play area into superchip RAM
 jsr CallBank1ToLoadCBSRAMwithLargeBitmap


 ;--- test bitsetting/detection bug:  - can't set 5th bit, bit 4!
; ldx #1 ;
; bne donetest
;testbitsetbug
; stx bitx
; lda #7
; sta bity

; txa
; pha

; lda #1
; jsr getbitstatus

; pla
; tax
; dex
; cpx #0
; bne testbitsetbug
;donetest
 ;---- end test



; jsr AbstractPlayfieldBuilder       ; ok in init...

 ;jsr framerelay
;------------------------------------------------------------
StartOfFrame ;-----------------------------------------------
;------------------------------------------------------------
    jsr framerelay ; do lots of stuff during a blank frame :)


    lda #0
    sta VBLANK
    lda #2
    sta VSYNC ; vertical sync signal; initiate electron guns to upper left corner!

    sta WSYNC ; 3 scanlines worth of vertical sync (so TV get's a lock on it)
    sta WSYNC
    sta WSYNC

    lda #0
    sta VSYNC ; vertical sync finished

    ;---------------------------------------------
    ; 37 Scanlines of Vertical Blank
    ;---------------------------------------------
    ldx #0

 ;DYNAMIC VERTICAL BLANK:
     ; 37 x 76 = 2812 ... /64 = 44 ; IT'S 43.94 - SHOULD i LDX 43 AND WSYNC AFTERWARDS?  
     ; YES OTHERWISE THAT'S 2816 .. 4 CYCLES OVER, 43 & TRAILING WSYNC SHOULD BE 2812

   ldx #43 ; #41;#43
   stx TIM64T

   ; 2740 cycles free for calls here ;)

; here's one, reverse byte subroutine, how many cycles?

 ;   lda #%11110101
  ;  sta PF2
   ; sta REVBT
   ; test reverse byte routine:
   ; jsr ReverseB 
    ;lda REVBT ; optional, accumulator already has the reverse byte
   ; sta PF1

  ; need to fit it here; building scrolling functionality (prefetch)
  ; in the bottom blank so need it each frame

  ;jsr AbstractPlayfieldBuilder ; it can fit here (cramped) but better in init


    lda #10
    sta PLAYFIELDINDEXstep ; count down to 0 and reset for each inc of index

;------------Music Engine Call:
;--------------------------------
 ;relocate later, sprite engine should go here, also needs optimization:

 jsr callbank1ForMusicEngine   ;= jsr PlayMusic, music engine is in bank1
;--------------------------------
;--------------------------------




   ; use remaining timeslice for electron beam

      lda #0
     sta a ; init temp var to 0 for sprite 0
     sta b ; init temp var to 0 for sprite 1
   ;  sta player1x ;! bug, this is overwriting screen buffer ram... check ram buffer DIRECTION
     ; lda #180
    ; sta player1y

    ;debug: these two lines turn off player0 (0,0 coordinates collapse sprite object)
 ;   sta player0x
 ;   sta player0y


;----------

;----------
 ;jmp MyVerticalBlank ;debug

     lda player0x ; desired position argument AND zero flag kick out
     beq skipplayer0horizontalsetup
     ldx #0 ; 0 for player1


 ;sta HMCLR
 


    jsr TheMagicRoutine ; :-)



skipplayer0horizontalsetup

;; jsr callbank1ForMusicEngine   ;= jsr PlayMusic, music engine is in bank1

 


; call horizontal positioning for 2nd sprite?
     lda player1x ; desired position argument AND zero flag kick out
     beq skipplayer0horizontalsetup2
     ldx #1 ; 1 for player2

 ;sta HMCLR   ; this clears HMPx fine tuners

   ; jsr PosObject
  ;   sta WSYNC
  ;sta HMOVE

    jsr TheMagicRoutine ; :-)

    ;STY HMP0,X               ; Fine positioning value
skipplayer0horizontalsetup2
;--
; call horizontal positioning for 3rd sprite?
     lda missile0x ; desired position argument AND zero flag kick out
     beq skipplayer0horizontalsetup3
     ldx #2 ; 2 for missile0

    jsr TheMagicRoutine ; :-)

    ;STY HMP0,X               ; Fine positioning value
skipplayer0horizontalsetup3

;--4
;--
; call horizontal positioning for 4th sprite
     lda missile1x ; desired position argument AND zero flag kick out
     beq skipplayer0horizontalsetup4
     ldx #3 ; 3 for missile1

    jsr TheMagicRoutine ; :-)

    ;STY HMP0,X               ; Fine positioning value
skipplayer0horizontalsetup4


         STA WSYNC
         STA HMOVE





MyVerticalBlank;          using timer instead of sta WSYNC
                    lda INTIM
    bne MyVerticalBlank



    STA WSYNC ; TRAILING WSYNC (USED 43 INSTEAD OF 44 FOR INTIM)











    ;---------------------------------------------
    ; 192 scanline playfield core ----------------
    ;---------------------------------------------
    ldx #96;   #0; #96; #0 ; framerelay is an entire frame;96x2=192
    ;;stx c; using temp var c to keep x free for indexing

;  push bitmap 40x10 playfield screen within core:
        
   ldy PLAYFIELDINDEX ; increment by six every 10 pixels so y is 10 pix high
   ; same as ldy #0 since it's initialised ; RAMplayfield offset

; sta WSYNC
; lda #0

 ;FIX
 sta WSYNC
 sta WSYNC



MyPlayfieldCore

 ;  hmmm... 192/2 = 96 addressable pixels (perfect)
 ;  with 152 (76*2) cycles
 ;  
 ;  count out manually before the trailer
 ;  ----


;                stx COLUPF ; playfield rainbow colour


   lda RAMplayfield,y ; y now points to MyPlayfield bitmap array
  ;;;  lda RAMplayfieldByte1
    sta PF0
;;    iny
  ;  lda RAMplayfieldByte2 ;
    lda RAMplayfield,y+1
    sta PF1
 ;;   iny
    lda RAMplayfield,y+2
    sta PF2
  ;;  iny

    ; wait until scanline is half way through...
   ; enough cycles already, 5+3+6+3+6+3=26

 ; inx
    lda RAMplayfield,y+3 ; y now points to MyPlayfield bitmap array
    sta PF0
  ;;  iny
    lda RAMplayfield,y+4
    sta PF1
 ;;   iny
    lda (RAMplayfield,y+5
    sta PF2
 ;;   iny

 ; 53 cycles so far

    ;------- end time between lines (76 cycles or less!)


; tya                         ;2
    dec PLAYFIELDINDEXstep     ; 6
    beq obtainnewposition ; 3   
    ;tya                   
 ;   pha
    sty c ; 3  - less cycles than tya pha... pla tay...
    bne stepovermtn ;
obtainnewposition ; 
  ;pha ;3
  ;GOTO NEXT PIXEL ROW:
  tya                     ;2
  clc                     ;2
  adc #6                  ;2
 ; tay                     
  ;pha                      ;4!0
  sta c                     ;3
  lda #10             ; 2
  sta PLAYFIELDINDEXstep  ; 3
  
stepovermtn


                  ;------------cycle border
 ;----- save these three cycles, already four cycles away:   sta WSYNC
;;; inx

 cpx missile0y  ; 3
 beq Missile0on  ;2
 lda #0          ;3
 beq missile0done  ;2
Missile0on
  lda #10           ;3
missile0done
 sta ENAM0           ;3



 lda #0   ;2

    sta PF0  ;3

    sta PF1  ;3

    sta PF2  ;3
                  ; accumulator already zero :
 cpx missile1y ;3
 bne missile1set ;2
 ;txa ;2      
 lda #11
missile1set
 sta ENAM1 ; 3

    ;-------------------------------------
    ; 11 cycles and counting!

 
 ;jmp jumparoundspriteinitb     ;debug
 

 ldy a ;3
 bne savecycles ; 3
 sty GRP0 ; clears sprite register when it's 0 ;)   ; 0 cycles
 beq jumparoundspritedetail  ;0 cycles
; lda #%01100101   ;TESTDATA, 1
savecycles  dey ; 2
;----------------19 cycles ... so far

 lda Sprite0SCR,y    ;       sprite0   ; 4 (absolute,y)
 ;lda  TESTDATA-1,y ;
 sta GRP0            ; 3
 sty a                ; 3
 bvc jumparoundspriteinit ;3
 ;jmp jumparoundspriteinit ;3
jumparoundspritedetail
  
;-----------------------32 cycles ... so far

 cpx player0y       ;3!0
 ;lda c
 ;cmp player0y
 bne jumparoundspriteinit ;2!0
 lda #8 ; 8x8 sprite matrice ; 2!0
 sta a                      ; 3!0
jumparoundspriteinit

;---------------------39 cycles
;debug:
;; jmp jumparoundspriteinitb ; save many cycles

;--sprite for player2
 ldy b                        ;3
; beq fixsprite2 ;fix;
 bne savecycles2 ;              3
 sty GRP1                       ; 0
 beq jumparoundspritedetailb   ;0
savecycles2 dey                           ;2
 lda Sprite1SCR,y;  ;          ;4
 ;lda TESTDATA-1,y
 sta GRP1                      ;3
 sty b                         ;3
 ;jmp jumparoundspriteinitb
 bvc jumparoundspriteinitb
jumparoundspritedetailb
;-------------------------------------57 cycles so far
 cpx player1y                   ; 3
 bne jumparoundspriteinitb       ; 2
 lda #8 ; 8x8 sprite matrice     ;2
 sta b                            ; 3
jumparoundspriteinitb
;--- 67 cycles
;--end sprite for player2

; pull y back
; pla          ;4
; tay          ;2
 ldy c




; nop
; nop
; nop
; nop

 ;nop

 ;nop
; nop
; nop
; nop
;-- 76 cycles ...... need to push some to the previous line


 ;nop
 ;nop
  sta WSYNC


   ; cpx #96; #192; #96 ; either offseting it at the top or the bottom of the loop changes the visible rainbow colours ;)
   ;cmp #96
  dex     ;2
 ;STA WSYNC
;    bne MyPlayfieldCore   ;3
   beq doneplayfieldcore
   jmp MyPlayfieldCore
doneplayfieldcore
;---------------------------------------------------
;-----Vertical Blank -------------------------------
;---------------------------------------------------
        lda #%01000010
        ;sta VBLANK
                sta VBLANK          ; vertical blank time after screen is drawn
        ldx #0


  ; DYNAMIC OVERSCAN:
     ; seeded with ? to match 30 calls to WSYNC?
 ; (30 x 76)/64 = 35.6 

     ldx #35
     stx TIM64T ; 


      ; These events all take time and should make use of timer regs
           ; dynamic timer should be seeded with one for scanline by scanline work

      ; ... done, there are 2000 cycles available:


   ; notenough time! relocate to the blank space above the screen:     jsr PlayMusic

 ;       jsr DanceToMusic
;  jsr pushabstractextendedplayfield



Overscan  ;DYNAMIC:


;  sta WSYNC
;        inx
;        cpx #30
   lda  INTIM

        bne Overscan
        sta WSYNC ; trailer


         jmp StartOfFrame


;---------------------------------
;---
;Optimised algorythm for positioning the 5 sprite objects horizontally:
;
;---- The Battlezone Horizontal Positioning Routine
TheMagicRoutine:
LD7E0: CMP    #$11    ;2
       BCS    LD7EA   ;2
              SBC    #$04    ;2
          BCS    LD7EA   ;2
          ADC    #$A5    ;2
LD7EA: STA    WSYNC   ;3
;;       STA    HMOVE   ;3
LD7EE: SBC    #$0F    ;2
    BCS    LD7EE   ;2 ->5 cycles per iteration!
        eor        #$07    ;2
        ASL            ;2
        ASL            ;2
        ASL            ;2
        ASL            ;2
        TAY            ;2

        STA    RESP0,X
        STa HMP0,X               ; Fine positioning value

      ;  STA    WSYNC   ;3
      ;  STA    HMOVE   ;3

        RTS            ;6



;---------------------------------
;---




;-------------------------------
; -- playfield screen data 40x10, 60 bytes
;-------------------------------
; don't think so, commenting this out:
; "That's what computes are for" - Dr Walter Gibs
;MyPlayfield
;  .byte %11111111,%11111111,%11111111,%00000000,%10101010,%00000011
;  .byte %00000000,%00000000,%00000000,%10000000,%10000000,%10000000
;  .byte %11110000,%11111111,%11111111,%11110000,%11111111,%11110011
;  .byte %00000000,%00000000,%00000000,%00000000,%10101011,%11110000
;        flip hb               flip     flip               flip hb
;  .byte %10110000,%00001111,%11000000,%00110000,%11110000,%00000000
;  .byte %00000000,%00000000,%00000000,%00000000,%00000000,%00000000
;  .byte %11111111,%10101010,%11111111,%00000000,%00000000,%00000000
;  .byte %00000000,%00000000,%00000000,%11111111,%11110111,%10111111
;  .byte %11111111,%11110011,%11111111,%00000000,%00000000,%00000000

  ; this row may be just a couple of scanlines short:

  ; .byte %11110000,%11111111,%11100111,%10110000,%11111111,%11101111

;MyAbstractExtendedPlayfield ; 20x10 grid (3 bytes) is read from a larger play area 5x as wide (12 bytes)
;;          1 0      4  2      12   3     20    4    28    5    36     6   44     7   52     8    60     9  68 72* 10   76    11    84   12
;  .byte %00000111, %11011111, %11011111, %00001111, %11101111, %11011111, %00001111, %11111011, %01000101, %01000111, %01000100, %01100111
;  .byte %00001011, %10101100, %10111011, %11111111, %11001111, %10101111, %00001111, %11111011, %01011101, %01010111, %01110111, %01010111
;  .byte %00001101, %01110111, %01000011, %11001111, %10000000, %00110111, %00001111, %11111000, %01000101, %01010111, %01000100, %11000011
;  .byte %00001110, %11111010, %11111011, %00001111, %11001110, %11111011, %00001111, %10011011, %01011101, %01010111, %01011111, %01110111
;  .byte %00001101, %01111101, %00000011, %11000111, %11111101, %11111101, %00001111, %01111011, %01000101, %01000111, %01000100, %01110111
;  .byte %00001011, %10111011, %01111011, %00001011, %11111011, %11111110, %10001110, %11111111, %11111111, %11111111, %11111111, %11111111
;  .byte %00000111, %11010111, %00111011, %11001101, %11110111, %11111111, %01001101, %01001111, %10111011, %11011111, %11111111, %11110001
;  .byte %00001011, %11101111, %01011011, %11001110, %11101111, %11111111, %10101011, %01001101, %10111010, %11011011, %01101110, %00111011
;  .byte %00001000, %11010111, %00000011, %00001111, %01011111, %11111111, %01010111, %01001001, %00010010, %10011011, %01100110, %00111011
;  .byte %00001111, %10111011, %11111111, %00001111, %10111111, %11111111, %00101111, %01001001, %00010010, %10011001, %01000010, %00111111


;-------------------------------------
;--WYSIWYG inline Sprite Library 
;-------------------------------------
; jsr loadplayer0, loadplayer1, y is the index argument
; (set y to 0/8/16/24/etc to load image 1/2/3/4/etc) 
; (target image is loaded upside down into high RAM)
;-------------------------------------
SPRITEDATA
;------------------sprite image 1 (ldy #0)
  .byte %00111000
  .byte %01000100
  .byte %00111000
  .byte %00010000
  .byte %01111100
  .byte %10111010
  .byte %10111001
  .byte %11000110
;------------------sprite image 2 (ldy #8)
  .byte %11111111
  .byte %10010001
  .byte %11111110
  .byte %10010100
  .byte %10011000
  .byte %10010000
  .byte %10100000
  .byte %11000000
;------------------sprite image 3 (ldy #16)
  .byte %11111111
  .byte %10000001
  .byte %10000001
  .byte %10000001
  .byte %10000001
  .byte %10000001
  .byte %10000001
  .byte %11111111


;------------------sprite image 4 (ldy #24)
  .byte %00000000
  .byte %01111110
  .byte %01000010
  .byte %01000010
  .byte %01000010
  .byte %01000010
  .byte %01111110
  .byte %00000000

;------------------sprite image 5 (ldy #32)
  .byte %00000000
  .byte %00000000
  .byte %00111100
  .byte %00100100
  .byte %00100100
  .byte %00111100
  .byte %00000000
  .byte %00000000

;------------------sprite image 6 (ldy #40)

  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00011000
  .byte %00011000
  .byte %00000000
  .byte %00000000
  .byte %00000000

;------------------sprite image 7 (ldy #48)
  .byte %00000000
  .byte %00000000
  .byte %00011000
  .byte %00100100
  .byte %00100100
  .byte %00011000
  .byte %00000000
  .byte %00000000

;------------------sprite image 8 (ldy #56)
  .byte %00000000
  .byte %00111100
  .byte %01000010
  .byte %01000010
  .byte %01000010
  .byte %01000010
  .byte %00111100
  .byte %00000000

;------------------sprite image 9 (ldy #64)
  .byte %01111110
  .byte %10000001
  .byte %10000001
  .byte %10000001
  .byte %10000001
  .byte %10000001
  .byte %10000001
  .byte %01111110

;------------------------------------
loadplayer0
;------------------------------------
; (set y argument to 0/8/16/24/etc to load image 1/2/3/4) 
; (target image is loaded upside down into high RAM)
;------------------------------------
 ldx #8
PushSprite0toCBSRAM lda SPRITEDATA,y ; get 1st byte of selected sprite from ROM image library
 iny
 dex
 sta Sprite0SCW,x ; put it in high RAM upside down 
 bne PushSprite0toCBSRAM
 rts
;---
loadplayer1
;------------------------------------
; (set y argument to 0/8/16/24/etc to load image 1/2/3/4) 
; (target image is loaded upside down into high RAM)
;------------------------------------
 ldx #8
PushSprite1toCBSRAM lda SPRITEDATA,y ; get 1st byte of selected sprite from ROM image library
 iny
 dex
 sta Sprite1SCW,x ; put it in high RAM upside down 
 bne PushSprite1toCBSRAM
 rts
;------------------------------------
;-- End Sprite Library
;------------------------------------



;TESTDATA
;  .byte %00011000
;  .byte %11111111
;  .byte %00001100
;  .byte %00001100
;  .byte %01011100
;  .byte %11111100
;  .byte %11111100
;  .byte %00001100
;  .byte %11111111
;  .byte %00011000

 ; .byte %00001000, %11101111, %00000011
 ; .byte %00001111, %00000001, %11111011
;  .byte %00001000, %10111101, %00000011
;  .byte %00001000, %10111101, %01111011
;  .byte %00001011, %11000001, %00111011
;  .byte %00001011, %11011100, %01011011
;  .byte %00001000, %00000001, %00000011
;  .byte %00001111, %11111111, %11111111


;MyAbstractPlayfield; - 20 x 10 pixel grid
 ; ------------ This gets transformed and bitflipped into a 60 byte array
 ; ------------ (double playfield pixel wide for 20x10)
 ; note: You can have 8 contiguous matrices and just adjust the offset (30x8=240)
 ; contiguous matrice1
;  .byte %00001111, %11111111, %11111111
;  .byte %00001100, %00000000, %01111011
;  .byte %00001000, %11101111, %00000011
;  .byte %00001111, %00000001, %11111011
;  .byte %00001000, %10111101, %00000011
;  .byte %00001000, %10111101, %01111011
;  .byte %00001011, %11000001, %00111011
;  .byte %00001011, %11011100, %01011011
;  .byte %00001000, %00000001, %00000011
;  .byte %00001111, %11111111, %11111111
 ; contiguous matrice2
;  .byte %11111111, %11111111, %11111111
;  .byte %10001100, %11111111, %00000011
;  .byte %10001010, %10000011, %00000011
;  .byte %10001111, %11011111, %11111111
;  .byte %10001000, %11100101, %00000011
;  .byte %10001000, %11110001, %01111011
;  .byte %10001111, %11111001, %00111011
;  .byte %10001001, %11111101, %01011011
;  .byte %10001001, %11111111, %00000011
;  .byte %11111111, %11111111, %11111111
;  .byte %00001101, %11110000, %00000011
 ; .byte %00001100, %11000000, %01111111
;  .byte %00000010, %10000000, %11100110
 ; .byte %00001111, %11000000, %11111111
  ;.byte %00001010, %11100011, %10101010
;  .byte %00001111, %11110000, %01111111
 ;; .byte %00000001, %11111000, %00111111
 ; .byte %00001111, %11111100, %00011111
 ; .byte %00000000, %11111110, %00001111
 ; .byte %00000000, %11111111, %00000111


;---------------------------------------------------------------


;---------------------------------------------------------
; Reverse Byte Routine 1100000 in REVBT becomes 00000011 
;---------------------------------------------------------
; Note: This routine is not used, comment it out if you need the space:
;;ReverseB ; REVBT holds byte to be reversed
;;   lda #0
;;   sta b
;;   lda REVBT
;;   and #%10000000 ; keep just the 8th bit
;;;is the 8th bit set?
;;   beq skip1
;;   ; reflect it to set bit 1
;;   lda b
;;  ora #%00000001 
;;  sta b ; put it back in b
;;skip1
;;  
;;  lda REVBT
;;  and #%0100000 ; keep just the 7th bit
;;; is the 7th bit set?
;;  beq skip2
;;  ;reflect it to set bit 2
;;  lda b
;;  ora #%00000010
;;  sta b
;;skip2
;;
;;  lda REVBT
;;  and #%00100000 ;keep just the 6th bit
;;  beq skip3 ; is the 6th bit set?
;;  lda b
;;  ora #%00000100; reflect to set bit 3
;;  sta b
;;skip3
;;
;; lda REVBT
;; and #%00010000 ; keep just the 5th bit
;; beq skip4; is the 5th bit set?
;; lda b
;; ora #%00001000; reflect to set bit 4
;; sta b
;;skip4
;;
;; lda REVBT
;; and #%00001000 ; keep just the 4th bit
;; beq skip5; is the 4th bit set?
;; lda b
;; ora #%00010000; reflect to set bit 5
;; sta b
;;skip5
;;
;; lda REVBT
;; and #%00000100 ; keep just the 3rd bit
;; beq skip6; is the 3rd bit set?
;; lda b
;; ora #%00100000; reflect to set bit 6
;; sta b
;;skip6
;;
;; lda REVBT
;; and #%00000010 ; keep just the 2nd bit
;; beq skip7; is the 2nd bit set?
;; lda b
;; ora #%0100000; reflect to set bit 7
;; sta b
;;skip7
;;
;; lda REVBT
;; and #%00000001 ; keep just the 1st bit
;; beq skip8
;; lda b
;; ora #%10000000; reflect to set bit 8
;; sta b
;;skip8
;;
;; lda b
;; sta REVBT ; put the reversed byte in REVBT and return
;;
;; rts
;;

;----------------------------
AbstractPlayfieldBuilder
;----------------------------
;start abstract playfield builder - pushes 20x10 bit grid blocks into RAM playfield 
; double size playfield pixels so 60 bytes of RAM from 30 bytes of ROM
;---------------------------------------------------------------------
; using $A0-BF$ (60 bytes) to hold playfield image (20x10 in 40x10 grid)
;-----------------------------
;end abstract playfieldbuilder
 lda #0 ; 30 bytes of ROM to read
 sta b ; using variable b to hold rom offset
 ;lda #0;  $A0 offset to point to start of playfield RAM grid
 sta c ; using variable c 


; lda #%10101010
 ldx #0
 stx b

;testfill works fine so prefill is off a byte!
;ok3 lda MyAbstractPlayfield,x
; sta $a0+30,x
; inx
; cpx 30
; bne ok3

;;testfill
;; ldx b
 ;sta $A0,b ; does nothing b is always 0! upper left only
;;  sta $a0,x
;; inc b
;; lda b
;; cmp #60
;; bne testfill
 ;sta 

                         



doit
 ldy #0 ; clear target

 tya ; preserve y
 ldy b
 ;ldx MyAbstractPlayfield,y; get rom playfield byte
 ldx $a0+#30,y; get rom playfield byte from RAM page it was dropped off at ;)


 ;  2 bits of first byte build 4 bits of pf0 target byte in memory
 ; next two bits of it build the first 4 bits of pf1,
 tay ; restore y

 txa
 and #%00001000 ; keep 1st bit (1/2 byte)
; is the 1st bit set?
 beq skip01
 tya
 ora #%00110000  ;set two playfield bits for 1 in pfo btye
 tay ; put it in y
skip01
 txa
 and #%00000100 ; keep 2nd bit           (half byte)
; is the 2nd bit set?
 beq skip02
 tya
 ora #%11000000 ; set next two playfield bits for 1 in pf0 byte
 tay
skip02
 
 txa ; preserve X in A
 ldx c
 sty $A0,x ; c write offset will be 60 when b read offset is 30
 tax ; restore X

 inc c ; on to next playfield byte target, pf1
; next two bits of read b go into the next c
 ldy #0 ; clear target

 txa ;x still has 1st byte from abstract ROM playfield
 and #%0000001 ; keep 3rd bit
 beq skip03
 tya
 ora #%00110000 ; set two bits for 1 in pf1 target byte
 tay
skip03
 txa ; get our byte back
 and #%00000010 ; keep 4th bit
 beq skip04
 tya
 ora #%11000000 ; set two bits for 1 in pf1 target byte
 tay
skip04
; ------- TRANSFORM 2nd of 3 source BYTES in the row
 inc b ; get 2nd byte
 tya ; preserve y
 ldy b
 ;ldx MyAbstractPlayfield,y; get rom playfield byte
 ldx $a0+#30,y ; ... from the RAM page it was dropped off at!
 tay ; restore y
 txa ; x now has 2nd byte (of 3 in row) from abstract ROM playfield
 and #%10000000 ; first bit set?
 beq skip05
 tya
 ora #%00001100 ; set two bits for 1 in pf1 target byte
 tay
skip05
 txa
 and #%01000000 ; check 2nd bit set
 beq skip06
 tya
 ora #%00000011 ; set two bits for 1 in pf1 target byte - DONE with PF1
 tay              ;* 20120801 this WAS the location of the col5 bug 00001111!
skip06
 txa ; preserve X in A
 ldx c

 sty $A0,x ; c write offset will be 60 when b read offset is 30
 tax
 inc c ; on to next playfield byte target, pf2
 ldy #0 ; clear target for 3rd byte write
 txa
 and #%00100000 ; check 3rd bit set
 beq skip07
 tya
 ora #%00000011; set two bits for 1 in pf2 target byte
 tay
skip07
 txa
 and #%00010000 ; check 4th bit set
 beq skip08
 tya
 ora #%00001100; set two bits for 1 in pf2 target byte
 tay
skip08
 txa
 and #%00001000; check 5th bit set
 beq skip09
 tya
 ora #%00110000; set two bits for 1 in pf2 target byte
 tay
skip09
 txa
 and #%00000100; 6th bit set?
 beq skip010
 tya
 ora #%11000000; set lasttwo bits for 1 in pf2 target byte
 tay
skip010
;-- on to next target byte pf0(2):
 txa ; preserve X in A
 ldx c
 sty $A0,x ; 3rd byte in row; c write offset will be 60 when b read offset is 30
 tax
 inc c ; on to next playfield byte target, pf0(2)
 ldy #0 ; clear target

 txa ; superfluous here ;)
 and #%00000010; 7th bit set? (2nd bit, had to reverse...)
 beq skip011
 tya
 ora #%00110000 ; set two bits for 1 in pf0(2)
 tay
skip011
 txa
 and #%00000001; 8th bit set? (1st bit, had to reverse order)
 beq skip012
 tya
 ora #%11000000 ; set two bits for 1 in pf0(2)
 tay
skip012
;-- on to next target byte pf1(2):
 txa ; preserve X in A
 ldx c
 sty $A0,x ; 4th byte in row; c write offset will be 60 when b read offset is 30
 tax ; restore X from A
 inc c ; on to next playfield byte target, pf1(2)
 ldy #0 ; clear target
 inc b ; get 3nd byte
 tya ; preserve y
 ldy b
 ;ldx MyAbstractPlayfield,y; get rom playfield byte
 ldx $a0+#30,y ; from the RAM page (2nd half of where we write too) it was dropped off at ;)
 tay ; restore y
 txa ; x now has 2nd byte (of 3 in row) from abstract ROM playfield
 and #%00010000 ; first bit set?
 beq skip013
 tya
 ora #%00000011 ; set two bits for 1 in pf1(2)
 tay
skip013
 txa
 and #%00100000 ; 2nd bit set?
 beq skip014
 tya
 ora #%00001100; set two bits for 1 in pf1(2)
 tay
skip014
 txa
 and #%01000000 ; 3rd bit set?
 beq skip015
 tya
 ora #%00110000; set two bits for 1 in pf1(2)
 tay
skip015
 txa
 and #%10000000; 4th bit set?
 beq skip016
 tya
 ora #%11000000; set two bits for 1 in pf1(2); done with it
 tay
skip016
 txa ; preserve X in A
 ldx c
 sty $A0,x ; 5th write; c write offset will be 60 when b read offset is 30
 tax ; restore x
 inc c ; on to next playfield byte target, pf2(2)
 ldy #0 ; clear target
 txa
 and #%00000001; 5th bit set?
 beq skip017
 tya
 ora #%11000000; set two bits for 1 in pf2(2)
 tay
skip017
 txa
 and #%00000010; 6th bit set?
 beq skip018
 tya
 ora #%00110000; set two bits for 1 in pf2(2)
 tay
skip018
 txa
 and #%00000100; 7th bit set?
 beq skip019
 tya
 ora #%00001100; set two bits for 1 in pf2(2)
 tay
skip019
 txa
 and #%00001000; 8th bit set?
 beq skip020
 tya
 ora #%00000011; set twobits for 1 in pf2(2); done with it
 tay
skip020
 txa ; preserve X in A
 ldx c
 sty $A0,x ;6th write c write offset will be 60 when b read offset is 30
 tax ; restore x
 inc c ; on to next playfield byte target, pf0 next 6 byte row


 inc b; get next 3 byte row
 lda b
 
 cmp #30
 ;bne doit
  bcs ok1done ;  beq ok1done is ok too but prefer branch if >=
 jmp doit
 ; rts
ok1done  ; done... needed a long jump
 ;jmp doit
 


 rts ;abstract playfield builder -------------------------

;-------------------
 ldy #0

; jmp pushabstractextendedplayfield
pushabs
 ldy #0
 ldx #0
pushabs2 lda MyAbstractExtendedPlayfield,x
 sta $a0+30,y
 iny
 inx
 lda MyAbstractExtendedPlayfield,x
 sta $a0+30,y
 iny
 inx
 lda MyAbstractExtendedPlayfield,x
 sta $a0+30,y

 iny
 txa
 clc
 adc #10
 tax
 cpy #30
 bne pushabs2
 rts


pushcondensedfield
; commenting this traditional routine out; It's all in the wrists - Flynn
; ldy #0
;pushcondensedfield2 ldx MyAbstractPlayfield,y
; stx $a0+#30,y ; put it 1/2 way down on target ram page
; iny
; cpy #30
; bne pushcondensedfield2
; rts

 ldy #0  ; what is this instruction doing here !?

; precalc byte index; inc x2 every 12 bits
;-------------------------------------
pushabstractextendedplayfield  ;------
;-------------------------------------
; BITIndex - determines how far to shift in to pull the 3 byte row:
; p0, p1, p2 - variables to hold the 3 byte row

 ldy #0
 ldx BYTErowoffset ; it starts with a row offset which steps the Y value each rowsize   ; #0
 ;---- BITIndex holds xbyte offset and it's target bit
 ; prep: MUST inc x offset 2 every 12 bits in BITIndex (keep subtracting
 ; 12 from bit index until it is less than 12.
 ; this allows handling for any bit pos since they overlap :)
 
; test BITIndex:   ! bit looping ... You can't read half the byte from a 4bit offset unless you loop 11 to 8
; lda #13   ; 9 is breaking...
; sta BITIndex

 ; inx ! bit looping ... and this is why inx does not work (reader expects a 1/2 byte)
 lda BITIndex; preserve:
 sta d;

 ;-- offset bitindex?
 ;-- 1 optimization: take 4 byte steps, fade to 1 byte steps
opt1 cmp #36
 bcc xoffsetbitindex
 inx
 inx
 inx
 inx
 sec
 sbc #32
 sta BITIndex ;--- end 1st optimization. Next: phy, use it in loops in place of mem
 jmp opt1

xoffsetbitindex ; --- yes, by creating a two byte index for every 12 bits
 cmp #12
 bcc xoffsetdone1 ;   <12 bits (0-11)
 ;;test inx ;
 inx ; offset x 
 sec ; before subtract!
 sbc #8
 sta BITIndex
 jmp xoffsetbitindex ; offset byte index again?

xoffsetdone1

paepf

 lda BITIndex
 bne bitpos1   
                    ; modify to use superchip: MyAbstractExtendedPlayfieldSCR

 ; ---BIT POS 0--- no work, just grab 3 bytes
 lda MyAbstractExtendedPlayfieldSCR,x
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x ; get 3rd byte in the row
 sta p2
 jmp bitposdone

bitpos1 ; shift the bytes:
 lda BITIndex
 cmp #1
 bne bitpos2


 ;! adressing mode is doing an add not a subtract, setting the carry and clearing it!
 ;!! That's where the bit's are going! 
 ;so...

 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0


; rol; asl ; shift acumulator left to grab the shifted bit in the carry flag


 jmp bitposdone ;---
 
bitpos2; shift 2 bits left
 lda BITIndex
 cmp #2
 bne bitpos3
 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0


 jmp bitposdone ;---

bitpos3;---

 lda BITIndex
 cmp #3
 bne bitpos4
 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 ;;;rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 jmp bitposdone ;---

bitpos4; --
 lda BITIndex
 cmp #4
 bne bitpos5
 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 jmp bitposdone ;---

bitpos5; --  not using 1st byte
 lda BITIndex
 cmp #5
 bne bitpos6

 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0


 jmp bitposdone; --


bitpos6; -- not using 1st byte
 lda BITIndex
 cmp #6
 bne bitpos7

 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 jmp bitposdone; --

bitpos7
 lda BITIndex
 cmp #7
 bne bitpos8

 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 jmp bitposdone; --

bitpos8; ---
 lda BITIndex
 cmp #8
 bne bitpos9

 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 jmp bitposdone; --

bitpos9 ;----
 lda BITIndex
 cmp #9
 bne bitpos10

 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

; 9th bit is in the next byte; repopulate c
 lda MyAbstractExtendedPlayfieldSCR,x+2
 sta c
 rol c
 rol p2
 rol p1
 rol p0


 jmp bitposdone; --

bitpos10;--
 lda BITIndex
 cmp #10
 bne bitpos11

 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

; 9th bit is in the next byte; repopulate c
 lda MyAbstractExtendedPlayfieldSCR,x+2
 sta c
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0


 jmp bitposdone; --

bitpos11;--
 lda BITIndex
 cmp #11
 bne bitpos12

 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

; 9th bit is in the next byte; repopulate c
 lda MyAbstractExtendedPlayfieldSCR,x+2
 sta c
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0
 rol c
 rol p2
 rol p1
 rol p0


 jmp bitposdone; --

bitpos12;--
 lda MyAbstractExtendedPlayfieldSCR,x+3 ; 4th byte holds a bit we need
 sta c ; store the 4th byte with that extra bit in c
 rol
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p0
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p1
 inx
 lda MyAbstractExtendedPlayfieldSCR,x
 ;rol
 sta p2
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0

; 9th bit is in the next byte; repopulate c
 lda MyAbstractExtendedPlayfieldSCR,x+2
 sta c
 rol c
 rol p2
 rol p1
 rol p0

 rol c
 rol p2
 rol p1
 rol p0
 rol c
 rol p2
 rol p1
 rol p0
 rol c
 rol p2
 rol p1
 rol p0

 ; bitpos12 (last one) falls through to... jmp bitposdone;------------


 ; now push bit shifted 3 bytes we've assembled from the larger row:


bitposdone ;-----------------------------------------
 lda p0
 sta $a0+#30,y ; put it 1/2 way down on target ram page
 iny


 lda p1
 sta $a0+#30,y ; put it 1/2 way down on target ram page
 iny


 lda p2
 sta $a0+#30,y ; put it 1/2 way down on target ram page
 iny
 txa
 clc ; forgot to do this and it inc'd!!!!!@#$
 adc #10
 tax ; point to next row
 cpy #30 ; overlooping
 ;bcc paepf out of range!
 bcs jumparoundpaepf
 jmp paepf
jumparoundpaepf
 lda d
 sta BITIndex ; restore bit index Y axis value
 rts
; end --- pushabstractextendedplayfield


; ----FRAMERELAY-----
; ----BLANKS A FRAME-
; ----DOES STUFF ;)--
; --00000000000000000000000000Replace this with 30 wysnc's + 30% of the screen's 192 scanline's worth of time...
; -- taking a portion of the screen should suffice instead.. see how it looks
; polling intim down from 114 perhaps ... (30 scanlines + 70 from the screen)
framerelay

    ; not like this - lda $fff9; use Bank 1 (CBS RAM), point back to bank 0 before JSR 

    lda #0
    sta VBLANK
    lda #2
    sta VSYNC ; vertical sync signal; initiate electron guns to upper left corner!
    sta WSYNC ; 3 scanlines worth of vertical sync (so TV get's a lock on it) 
    sta WSYNC
    sta WSYNC

    lda #0
    sta VSYNC ; vertical sync finished
    sta PF1
    sta PF0
    sta PF2 ; clear playfield registers for blank frame

    sta GRP0 ; clear sprite 0 for blank frame
    sta GRP1
   ; sta ENAM1
   ; sta ENAM0

    ; 37 scanlines ofvertical  blank  ...  37 x 76 = 2812 ... /64 = 44
    ;+192 scanlines of screen  = (229 * 76)/64 = 271.9 (271 + trailing WSYNC) 
    ; ------- break that into 255 and 16 ;)
    ldx #17
    stx TIM64T ;
  ;--- note: 76x17 cycles available right here:
  ;--- you could put a small block of code here if you need more time:


  ;--- end small block of code 


w16 lda INTIM
 bne w16

    ldx #255  ; 
    stx TIM64T ; big block of time now! Can put the kitchen sink here if you want :)
   ; ---- time intensive calls go here!
   ; playfield setup and builder calls, et al.


  ;jsr pushcondensedfield
  ;

 ; ----------------------------------------------------------
 ; ------ Your Abstract Assembly code goes here:
 ; ----------------------------------------------------------
 ;
 ;-------------------------------------------------------
 ; ASSEMBLY BLOCK GENERATED BY bB:

.L012 ;  rem ** move sprites back and forth vertically **
.L013 ;  rem send player one all the way across the screen:
.L014 ;  if player1x  =  150 then player1x = 0 : goto jumparound

	LDA player1x
	CMP #150
     BNE .skipL014
.condpart0
	LDA #0
	STA player1x
 jmp .jumparound
.skipL014
.L015 ;  player1x  =  player1x  + 1
	INC player1x
.jumparound
 ; jumparound

.
 ; 

.
 ; 

.L016 ;  rem send player0 half way across:

.L017 ;  if player0x  =  75 then player0x = 0 : goto jumparound2

	LDA player0x
	CMP #75
     BNE .skipL017
.condpart1
	LDA #0
	STA player0x
 jmp .jumparound2

.skipL017
.L018 ;  player0x  =  player0x  + 1

	INC player0x
.jumparound2
 ; jumparound2

.
 ; 


 ;---- SPRITE ANIMATION DEMO IMPORTED FROM bB COMPILER:
.L019 ;  rem ** sprite animation demo:

.L020 ;  rem ** flip through several sprite frames

.L021 ;  rem ** using variable f

.L022 ;  rem *** using dummy variable c as a place holder to call an ASDK function

.L023 ;  rem ** (the ASDK supports variables a-q but a-e are temp vars)
.
 ; 

.L024 ;  c = 0 :  rem dummy variable c 

   ldy f   ;    LDA #0
   jsr loadplayer0    ;    STA c
.
 ; 

.L025 ;  f = f + 8 :  set offset argument to point to next image

	LDA f
	CLC
	ADC #8
	STA f
.L026 ;  if f  =  72 then f  =  24

	LDA f
	CMP #72
     BNE .skipL026
.condpart2
	LDA #24
	STA f
.skipL026
.
 ; 




 ;---------------------------------------------------------
 ;---------------------------------------------------------

;--"Drawscreen:"
;-- Now Call the Twin Engines that emulate hardware level
;-- Horizontal Scrolling and Scaling
;----------------------------------------slide view window along bitmapped panorama currently loaded into CBS RAM:

; call primary rending engine:

 jsr pushabstractextendedplayfield

;-- any code that needs to access the double buffer in low RAM can go here
;-- before calling the secondary rendering engine:
                                                  
;--------- call 2ndary rendering engine:
;--------- expand and flip 30 bytes of system RAM buffer into 60 Bytes for display:

 jsr AbstractPlayfieldBuilder

;-------------------------------------------


 ;-----------------------
 ;----------------------------


; ------------------------------------------
; ---- Resume Framework
;-------------------------------------------
                
w17 lda INTIM
 bne w17 ; done with large block of time!


 ;AGAIN, 2 WSYNC PATCH ... NO IDEA WHERE THESE WERE LOST TO; ROUNDING PERHAPS
 LDA #0
 STA WSYNC
 STA WSYNC
 STA WSYNC

    ;+30 scanlines of vertical blank  ; (37 x 76)/64 = 35.6 

;-----Vertical Blank -------------------------------
;---------------------------------------------------
        lda #%01000010
                sta VBLANK          ; vertical blank time after screen is drawn
        ldx #0


  ; DYNAMIC OVERSCAN:
     ; seeded with ? to match 30 calls to WSYNC?
 ; (37 x 76)/64 = 35.6 

     ldx #35
     stx TIM64T ;  -------- time for more calls here:



Overscan2  ;DYNAMIC:
   lda  INTIM

        bne Overscan2
        sta WSYNC ; trailer

 rts ;--------------done with blank frame (framerelay)
;------------------------------------------------------------
;------------END FRAMERELAY----------------------------------
;------------------------------------------------------------
;------------------------------------------------------------






;-------------------------------------------------------------------------------------
 ;----GetBitStatus (subroutine/function) gets or sets bit status; accumulator passes the argument
 ; vars a and b used as temp vars; initially used the stack (preferred) but
 ; revised on debugging and never pushed it back ;)
 ; (so vars a and b will be overwritten if you use them in your code)

getbitstatus
 ; ... this should be a dual get/set routine.
; arguments passed via the accumulator (lda #arg [0,1])
; always do this... lda #0 to flip the target bit via inversion, will clear or set it.
; lda #1 for setting it, also returns it's prior state in the accumulator, 0 if it was previously off


 sta a ; keep get/set flag that was passed in the accumulator
 ldx #0                      


 lda bitx
xoffsetgetbitstatus
 cmp #12
 bcc xoffsetdone_getbitstatus
 inx
 sec
 sbc #8
 jmp xoffsetgetbitstatus
xoffsetdone_getbitstatus ; x byte offset in x, bit offset in accumulator

 ;offset a single byte?
 cmp #4
 bcc donesinglebyteoffset
 inx
donesinglebyteoffset
 sta b ; preserve bit offset
 ldy bity
flatten_y cpy #0
 beq y_flat
 dey
 txa
 clc
 adc #12 ; add one row per y pixel
 tax
 jmp flatten_y

y_flat ; x now points to cell, bit pointer is in the stack

 lda b ; -- get bitpointer
     ; -- Compare it against 12 handlers (first 4 are special, next 8 are reused for any subsequent bit index)



        ; -- pull from similar fillout routine; first add and test superchip large playfield in RAM, store 240 bytes in 120 ;)
; beq getset0
 bne getset1
 ; fall through to 0 should not happen

getset0
  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%00001000 ; was it set?
  sta b ; push previous bit status 
  tya
  ldy a ; get/set status is in var a, 0 clears bit
  beq clear0
  ora #%00001000 ; set it
  cpy #0
  bne done0; branch always
clear0
  eor #%00001000 ; clear it
done0  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset1
 cmp #1
 bne getset2

  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%00000100 ; was it set?
  sta b ;pha ; push previous bit status
  tya
  ldy a ; get/set status is in var a, 0 clears bit
  beq clear1
  ora #%00000100 ; set it
  cpy #0
  bne done1; branch always
clear1
  eor #%00000100 ; clear it
done1  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset2
  cmp #2
  bne getset3

  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%00000010 ; was it set?
  sta b;pha ; push previous bit status
  tya
  ldy a ; get/set status is in var a, 0 clears bit
  beq clear2
  ora #%00000010 ; set it
  cpy #0
  bne done2; branch always
clear2
  eor  #%00000010
done2  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset3
  cmp #3
  bne getset4
  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%00000001 ; was it set?
  sta b;pha ; push previous bit status
  tya
  ldy a
  beq clear3
  ora #%00000001 ; set it
  cpy #0
  bne done3
clear3
  eor #%00000001 ; clear it
done3  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset4
  cmp #4
  bne getset5
  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%10000000 ; was it set?
  sta b;pha ; push previous bit status
  tya
  ldy a ; clear or set toggle
  beq clear4
  ora #%10000000 ; set it
  cpy #0
  bne done4
clear4
  eor #%10000000  ; clear it
done4 sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset5
  cmp #5
  bne getset6
  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%01000000 ; was it set?
  sta b;pha ; push previous bit status
  tya
  ldy a ; clear or set toggle
  beq clear5
  ora #%01000000 ; set it
  cpy #0
  bne done5;branch always
clear5 eor #%01000000 ; clear it
done5  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset6
  cmp #6
  bne getset7
  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%00100000 ; was it set?
  sta b;pha ; push previous bit status
  tya
  ldy a ; clear or set toggle
  beq clear6

  ora #%00100000 ; set it
  cpy #0
  bne done6
clear6 eor #%00100000; clear it
done6  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset7
 cmp #7
 bne getset8
  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%00010000 ; was it set?
  sta b;pha ; push previous bit status
  tya
  ldy a ; clear or set toggle
  beq clear7
  ora #%00010000 ; set it
  cpy #0
  bne done7
clear7 eor #%00010000
done7  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset8
 cmp #8
 bne getset9
  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%00001000 ; was it set?
  sta b;pha ; push previous bit status
  tya
  ldy a ; clear or set toggle
  beq clear8
  ora #%00001000 ; set it
  cpy #0
  bne done8
clear8 eor #%00001000
done8  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset9
 cmp #9
 bne getset10
  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%00000100 ; was it set?
  sta b;pha ; push previous bit status
  tya
  ldy a ; clear or set toggle
  beq clear9
  ora #%00000100 ; set it
  cpy #0
  bne done9; branch always
clear9 eor #%00000100
done9  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset10
  cmp #10
  bne getset11
  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%00000010 ; was it set?
  sta b; pha ; push previous bit status
  tya
  ldy a ; clear or set toggle
  beq clear10

  ora #%00000010 ; set it
  cpy #0
  bne done10;branch always
clear10 eor #%00000010
done10  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
  jmp donegetset

getset11     ; fall through should never happen...
  ldy MyAbstractExtendedPlayfieldSCR,x  ;  read area of superchip
  tya
  and #%00000001 ; was it set?
  sta b;pha ; push previous bit status
  tya
  ldy a ; clear or set toggle
  beq clear11
  ora #%00000001 ; set it
  cpy #0
  bne done11; branch always
clear11 eor #%00000001
done11  sta MyAbstractExtendedPlayfieldSCW,x ; store in write area of SUPERCHIP
 ; jmp donegetset

donegetset
 lda b; pla ; return previous bit status
 rts ; getbitstatus -------------------------------

 ;----SetBitStatus (SCROLLOUT)
setbitstatus
 nop ; not used, above routine handles both
 nop
 rts


;;InterruptVectors ... not now


;---------------------------------------------------
;---------------------------------------------------
		
	org $DFFC
	rorg $FFFC
	.word Bank0
        .byte "B0"

;---------------------------------------------------
;---------------------------------------------------
;---------------------------------------------------



      ;--------bank 1 is used to hold the large virtual world bitmaps and the musical score and music engine:
	seg bank_1
	org $E000
	rorg $F000
	
	repeat 512
	.byte $00
	repend
	
Bank1
;        lda $FFF8
 nop
 nop
 nop
 jmp jmparoundcallbank1stub
callbank1ForMusicEngineRecvd
 lda $1000 ; dummy placeholder, never executes
 jsr PlayMusic ; executes
 lda $fff8 ; go back to bank 0, same spot
 rts ;never executes

CallBank1ToLoadCBSRAMwithLargeBitmapRecvd
 lda $1000; dummy placeholder, never executes
 jsr LoadLargeBitMapIntoCBSRAM ; executes
 lda $fff8 ; go back to bank 0, same spot
 rts ; never executes

CallBank1ToLoadCBSRAMwithLargeBitmapSayingREADYPLAYER1recvd
 lda $1000; dummy placeholder, never executes
 jsr LoadLargeBitMapIntoCBSRAMSayingREADYPLAYER1 ; executes
 lda $fff8 ; go back to bank 0, same spot
 rts ; never executes

CallBank1ToLoadCBSRAMwithGameScreenBitmaprecvd
 lda $1000; dummy placeholder, never executes
 jsr LoadLargeBitMapIntoCBSRAMforGameScreen ; executes
 lda $fff8 ; go back to bank 0, same spot
 rts ; never executes


jmparoundcallbank1stub
;------------------------------------------------

LoadLargeBitMapIntoCBSRAM
; -----  Demo and Scrollout virtual world
 ldx #239;#120 ; expand to 240 byte table with CBS RAM!
pushlargeplayareaintosc lda MyAbstractExtendedPlayfield,x
 sta MyAbstractExtendedPlayfieldSCW,x
 dex
 bne pushlargeplayareaintosc
 sta MyAbstractExtendedPlayfieldSCW,x ; missed the last byte
 rts

LoadLargeBitMapIntoCBSRAMSayingREADYPLAYER1
; -----  Demo and Scrollout virtual world
 ldx #239;#120 ; expand to 240 byte table with CBS RAM!
pushlargeplayareaintosc2 lda ReadyPlayerOneBanner,x
 sta MyAbstractExtendedPlayfieldSCW,x
 dex
 bne pushlargeplayareaintosc2
 sta MyAbstractExtendedPlayfieldSCW,x ; missed the last byte
 rts

LoadLargeBitMapIntoCBSRAMforGameScreen
; -----  Demo and Scrollout virtual world
 ldx #239;#120 ; expand to 240 byte table with CBS RAM!
pushlargeplayareaintosc3 lda GameScreenBitmap,x
 sta MyAbstractExtendedPlayfieldSCW,x
 dex
 bne pushlargeplayareaintosc3
 sta MyAbstractExtendedPlayfieldSCW,x ; missed the last byte
 rts


;--------------- SCROLLOUT DEMO AND PLAY AREA: ---------------------------
;------------Large Play Area 92x20; panned by a 20x10 virtual pixel screen
;------------480 total bytes of screen RAM are stored in 240 bytes and panned, flipped and expanded in real time
;------------It also gets flipped right ways up so WYSIWYG in the virtual world below :)

MyAbstractExtendedPlayfield ; 20x10 grid (3 bytes) is read from a larger play area 5x as wide (12 bytes) and twice as tall
;          1 0      4  2      12   3     20    4    28    5    36     6   44     7   52     8    60     9  68 72* 10   76    11    84   12
  .byte %00001111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001
;-- next 10 rows is another screen down and 5 across:
  .byte %00001111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %11010111
  .byte %00001000, %00000000, %00000000, %11110000, %00011000, %00000000, %11111100, %00000000, %00000000, %00000000, %00000001, %01010111
  .byte %00001000, %00000000, %00000000, %11100000, %00111100, %00000000, %10000101, %11110111, %11011111, %00100000, %00000000, %11010111
  .byte %00001000, %00000000, %00000000, %00000000, %00011000, %00000000, %11111100, %01000100, %01010001, %00100000, %00000001, %01010111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %11000100, %01100111, %11011111, %10110000, %00000000, %11010111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %11000100, %01100110, %01011000, %10110000, %00000001, %01010111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %11000100, %01100110, %01011000, %10110000, %01110111, %11010111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01110111, %01010111
  .byte %00001111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111

;-- -- -- note: virtual world bitmaps are contiguous and WYSIWYG, but the first 4 pixels
;-- -- -- of just the first byte won't display (spacer padding artifact left in place, not needed!)
;-- -- -- see how the R in the ready player one banner below never shows up:

ReadyPlayerOneBanner ; 20x10 grid (3 bytes) is read from a larger play area 5x as wide (12 bytes) and twice as tall
;          1 0      4  2      12   3     20    4    28    5    36     6   44     7   52     8    60     9  68 72* 10   76    11    84   12
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001, %01000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000010, %00100000, %01111110
  .byte %01100111, %01110110, %00101000, %11001000, %11101010, %11100110, %00011101, %00101110, %01010000, %00000100, %00010000, %00111100
  .byte %01010100, %01010101, %00010000, %10101000, %10100100, %10000101, %00010101, %10101000, %01010000, %00001000, %00001000, %00011000
  .byte %01100111, %01110100, %10010000, %11001000, %11100100, %11100110, %00010101, %01101110, %01010000, %00010000, %00000100, %00000000
  .byte %01010100, %01010101, %00010000, %10001000, %10100100, %10000101, %00010101, %00101000, %00000000, %00001000, %00001000, %11000010
  .byte %01010111, %01010110, %00010000, %10001110, %10100100, %11100101, %00011101, %00101110, %01010000, %00000100, %00010011, %11000110
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000010, %00100111, %11001110
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000001, %01000000, %00011110
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %10000000, %00111110
;-- next 10 rows is another screen down and 5 across:
  .byte %00001111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %11010111
  .byte %00001000, %00000000, %00000000, %11110000, %00011000, %00000000, %11111100, %00000000, %00000000, %00000000, %00000001, %01010111
  .byte %00001000, %00000000, %00000000, %11100000, %00111100, %00000000, %10000101, %11110111, %11011111, %00100000, %00000000, %11010111
  .byte %00001000, %00000000, %00000000, %00000000, %00011000, %00000000, %11111100, %01000100, %01010001, %00100000, %00000001, %01010111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %11000100, %01100111, %11011111, %10110000, %00000000, %11010111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %11000100, %01100110, %01011000, %10110000, %00000001, %01010111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %11000100, %01100110, %01011000, %10110000, %01110111, %11010111
  .byte %00001000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01110111, %01010111
  .byte %00001111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111

;; -- game screen:
GameScreenBitmap ; 20x10 grid (3 bytes) is read from a larger play area 5x as wide (12 bytes) and twice as tall
;          1 0      4  2      12   3     20    4    28    5    36     6   44     7   52     8    60     9  68 72* 10   76    11    84   12
  .byte %00000000, %00000000, %00101101, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101101, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101001, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101001, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101001, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00100101, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00100101, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00100101, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101001, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101001, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
;-- next 10 rows is another screen down and 5 across:
  .byte %00000000, %00000000, %00101101, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101001, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101001, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101101, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00100101, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00100101, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101001, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101001, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00100101, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111
  .byte %00000000, %00000000, %00101001, %01100001, %00000111, %00000001, %00010111, %00001111, %00001111, %00000000, %00000000, %00000111


;-------------------------------------------------------------
; Background Music Engine -------------------------
;-------------------------------------------------------------
PlayMusic

;   pha ; push a
;   tya
;   pha ; push y

 ;jmp musicfinish ;fix timing? yes, reduce it!

   lda SUSTAINFORFRAMES

   cmp #0
   bne waitnextnote 

;get next notes:
   lda e
   cmp #1
   bne continuenote
   ldy MUSICINDEX
   iny
   iny
   jmp skipnote
continuenote



   lda MUSICINDEX
   tay
   lda MusicData,y
   sta AUDV0 ; volume Oscillator 0
   iny
   lda MusicData,y
   sta AUDC0 ; audio wave type Oscillator 0
   iny
   lda MusicData,y
   sta AUDF0 ;audio frequency Oscillator 0
   ;-------------
skipnote   ;-- substitute sound fx forvoice 0 (overwrite)?
   iny
   lda MusicData,y
   sta AUDV1 ; volume Oscillator 0
   iny
   lda MusicData,y
   sta AUDC1 ; audio wave type Oscillator 0
   iny
   lda MusicData,y
   sta AUDF1 ;audio frequency Oscillator 0
   iny
   lda MusicData,y
   sta SUSTAINFORFRAMES
   cmp #0; 0 duration signals reset to start of tune
   bne jump1
   sta MUSICINDEX ;initialize and start over
   
   jmp PlayMusic

jump1

   
   iny ; point to next data line (for musical score) in the table
   sty MUSICINDEX

waitnextnote
   ldy SUSTAINFORFRAMES
   dey
   sty SUSTAINFORFRAMES ;decrement framedelay counter
   bne continuewaitnextnote
   ldy #0
   sty e ; clear sound effect if set
continuewaitnextnote
   ldy e
   beq musicfinish
   ldy #1
   cpy SUSTAINFORFRAMES
   bcs musicfinish
   sty SUSTAINFORFRAMES
musicfinish   rts

;-------------------------------------
; Music Data -------------------------
;-------------------------------------------------------------
; volume0,wave,freq,volume1,wave,freq,framesduration (0 duration loops it) 
;-------------------------------------------------------------
MusicData
        .byte 14,6,8,14,6,11,10
        .byte 14,6,29,14,6,20,80
        .byte 7,6,7,14,6,20,10
        .byte 3,6,29,3,6,20,15   
        .byte 1,15,3,1,7,24,10
        .byte 14,8,25,14,8,31,5
        .byte 1,8,25,1,8,31,2
        .byte 14,8,25,14,8,31,5
        .byte 1,8,25,1,8,31,2
        .byte 14,8,25,14,8,31,5
        .byte 1,8,25,1,8,31,2
        .byte 14,8,25,14,8,31,5                          
        .byte 1,8,25,1,8,31,2
        .byte 14,6,8,14,6,8,5
        .byte 14,6,8,14,6,8,5
        .byte 1,8,25,1,8,31,2    
        .byte 14,6,11,14,6,11,5
        .byte 1,8,25,1,8,31,2
        .byte 14,6,8,14,6,11,10
        .byte 1,8,25,1,8,31,50
        .byte 1,8,25,1,8,31,0




      ;  .byte 3,8,1,3,3,1,21
      ;  .byte 14,2,2,14,2,1,20
      ;  .byte 1,15,10,1,7,4,0
      ;  .byte 5,5,1,4,3,21,0    ; ,0 loops!
      ;  .byte 5,8,1,4,3,21,20
      ;  .byte 5,11,1,4,3,21,30
      ;  .byte 5,15,10,3,7,24,40
      ;  .byte 10,5,1,4,3,21,50
      ;  .byte 10,8,1,4,3,21,70
      ;  .byte 15,21,1,10,21,1,90
      ;  .byte 10,11,1,4,3,1,0 ; ,0 - and loop


;---------------------------------- end bank 1 code


	

	org $EFFC
	rorg $FFFC
	.word Bank1
	.byte "B1"

       ;----------------BANK2, NOT IN USE:
	seg bank_2
	org $F000
	rorg $F000
	
	repeat 512
	.byte $00
	repend
	
Bank2
	
	lda $FFF8
	

	org $FFFC
	rorg $FFFC
	.word Bank2
	.byte "B2"
