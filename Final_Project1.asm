;timer 0: to gerenate the sounds, only generates 2048 HZ at the moment
;timer 2: to measure the frequency

;Functionalities
; 1. Flashing musical game intro
; 2. Game instructions shown with high and low frequency sounds
; 3. Song 1- Tokyo Drift
; 4. Song 2 - Lucid Dreams
; 5. Piano
; 6. Pause button - pause for a few seconds when the game is going on. Goes back to the game automatically

$NOLIST
$MODLP51
$LIST 

INPIN1 equ P2.0 
INPIN2 equ P2.1
SOUND_OUT equ P1.1
BOOT_BUTTON   equ P4.5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE0  EQU ((2048*2)-200)
TIMER0_RATE1  EQU ((2048*2))
TIMER0_RELOAD0 EQU ((65536-(CLK/TIMER0_RATE0)))
TIMER0_RELOAD1 EQU ((65536-(CLK/TIMER0_RATE1)))
NOTE_B         EQU  (988)
NOTE_B_RELOAD  EQU ((65536-(CLK/NOTE_B)))
NOTE_Bf         EQU  (932)
NOTE_Bf_RELOAD  EQU ((65536-(CLK/NOTE_Bf)))
NOTE_Ef         EQU  (1244)
NOTE_Ef_RELOAD  EQU ((65536-(CLK/NOTE_Ef)))

NOTE_A         EQU  (1760)
NOTE_A_RELOAD  EQU ((65536-(CLK/NOTE_A)))
NOTE_Db         EQU  (1109)
NOTE_Db_RELOAD  EQU ((65536-(CLK/NOTE_Db)))
NOTE_Ab         EQU  (1661)
NOTE_Ab_RELOAD  EQU ((65536-(CLK/NOTE_Ab)))
NOTE_Fs         EQU  (1480)
NOTE_Fs_RELOAD  EQU ((65536-(CLK/NOTE_Fs)))
NOTE_B5         EQU  (1476)
NOTE_B5_RELOAD  EQU ((65536-(CLK/NOTE_B5)))
NOTE_F         EQU  (1397)
NOTE_F_RELOAD  EQU ((65536-(CLK/NOTE_F)))

org 0000H
    ljmp Startup
    
org 0x000B   ; Timer/Counter 0 overflow interrupt vector
    ljmp Timer0_ISR
    
; These register definitions needed by 'math32.inc'
DSEG at 30H
x:   ds 4
y:   ds 4
Seed: ds 4
bcd: ds 5
bcd_1: ds 5
T2ov: ds 2 ; 16-bit timer 2 overflow (to measure the period of very slow signals)
Period_A: ds 5
Period_B: ds 5
score_1: ds 2
score_2: ds 2


BSEG
mf: dbit 1
HF: dbit 1
LF: dbit 1
zero: dbit 1
HLbit: dbit 1

$NOLIST
$include(math32.inc)
$LIST

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST
 
;                     1234567890123456    <- This helps determine the location of the counter
Player1: db  'Player 1: ', 0
Player2: db  'Player 2: ', 0
Winner:  db  'Winner!', 0
Loser:   db  'Loser:(', 0
Welcome: db   'Welcome to fast', 0
Welcome2: db   'and furious!', 0
Erase:    db   '                ', 0
Paused:    db   'Paused', 0
high_freq:  db  'This is high freq',0
low_freq:  db  'This is low freq',0 
high_freq_instr:  db 'Slap = +1 point!',0
low_freq_instr:  db 'Slap = -1 point!',0
piano_message1:  db 'Piano mode',0
piano_message2:  db 'Play notes A & B',0


;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init_0:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD0)
	mov TL0, #low(TIMER0_RELOAD0)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD0)
	mov RL0, #low(TIMER0_RELOAD0)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048+100 or -100 Hz square wave at pin P1.1 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	cpl SOUND_OUT ; Connect speaker to P1.1!
	reti

;Initializes timer/counter 2 as a 16-bit timer
InitTimer2:
	mov T2CON, #0b_0000_0000 ; Stop timer/counter.  Set as timer (clock input is pin 22.1184MHz).
	; Set the reload value on overflow to zero (just in case is not zero)
	mov RCAP2H, #0
	mov RCAP2L, #0
    ret		
			
Random:
       ; Seed = 214013 * Seed + 2531011
mov x+0, Seed+0
mov x+1, Seed+1
mov x+2, Seed+2
mov x+3, Seed+3
    Load_y(214013)
lcall mul32
    Load_y(2531011)
lcall add32
mov Seed+0, x+0
mov Seed+1, x+1
mov Seed+2, x+2
mov Seed+3, x+3
    ret

Wait_random: ; wait a random time between 0ms and 1024ms
Wait_Milli_Seconds(Seed+0) 
Wait_Milli_Seconds(Seed+1)
Wait_Milli_Seconds(Seed+2)
Wait_Milli_Seconds(Seed+3)
ret

wait_for_P4_5:
	jb P4.5, $ ; loop while the button is not pressed
	Wait_Milli_Seconds(#50) ; debounce time
	jb P4.5, wait_for_P4_5 ; it was a bounce, try again
	jnb P4.5, $ ; loop while the button is pressed
	ret

Wait1s:
    mov R2, #176
X3: mov R1, #250
X2: mov R0, #166
X1: djnz R0, X1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, X2 ; 22.51519us*250=5.629ms
    djnz R2, X3 ; 5.629ms*176=1.0s (approximately)
    ret

;code for first song - Tokyo Drift from fast and furious
F_n_f:
    
    Set_Cursor(1, 1)
	Send_Constant_String(#Welcome)
	Set_Cursor(2, 1)
	Send_Constant_String(#Welcome2)
	clr TR0
	mov RH0, #high(NOTE_Bf_RELOAD)
	mov RL0, #low(NOTE_Bf_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#150)
	Wait_Milli_Seconds(#150)
	
	Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
	cpl TR0 
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
		
	setb TR0
	Set_Cursor(1, 1)
	Send_Constant_String(#Welcome)
	Set_Cursor(2, 1)
	Send_Constant_String(#Welcome2)
	clr TR0
	mov RH0, #high(NOTE_Bf_RELOAD)
	mov RL0, #low(NOTE_Bf_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#150)
     Wait_Milli_Seconds(#150)
	Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
	cpl TR0
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)


here:	
	setb TR0
	Set_Cursor(1, 1)
	Send_Constant_String(#Welcome)
	Set_Cursor(2, 1)
	Send_Constant_String(#Welcome2)
	clr TR0
	mov RH0, #high(NOTE_Bf_RELOAD)
	mov RL0, #low(NOTE_Bf_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#150)
     Wait_Milli_Seconds(#150)
	Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
	cpl TR0
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)


	
	setb TR0
	Set_Cursor(1, 1)
	Send_Constant_String(#Welcome)
	Set_Cursor(2, 1)
	Send_Constant_String(#Welcome2)
	clr TR0
	mov RH0, #high(NOTE_Bf_RELOAD)
	mov RL0, #low(NOTE_Bf_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#150)
     Wait_Milli_Seconds(#150)
	Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
	cpl TR0
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)


	
	setb TR0
	Set_Cursor(1, 1)
	Send_Constant_String(#Welcome)
	Set_Cursor(2, 1)
	Send_Constant_String(#Welcome2)
	clr TR0
	mov RH0, #high(NOTE_Bf_RELOAD)
	mov RL0, #low(NOTE_Bf_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#150)
    Wait_Milli_Seconds(#150)
	Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
	cpl TR0
	Wait_Milli_Seconds(#75)

	
	setb TR0
	Set_Cursor(1, 1)
	Send_Constant_String(#Welcome)
	Set_Cursor(2, 1)
	Send_Constant_String(#Welcome2)
	clr TR0
	mov RH0, #high(NOTE_B_RELOAD)
	mov RL0, #low(NOTE_B_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#150)
    Wait_Milli_Seconds(#150)
	Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
	cpl TR0
	Wait_Milli_Seconds(#75)
	

	
	setb TR0
	Set_Cursor(1, 1)
	Send_Constant_String(#Welcome)
	Set_Cursor(2, 1)
	Send_Constant_String(#Welcome2)
	clr TR0
	mov RH0, #high(NOTE_Ef_RELOAD)
	mov RL0, #low(NOTE_Ef_RELOAD)
	setb TR0
	Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
	Wait_Milli_Seconds(#150)
	Wait_Milli_Seconds(#150)
	
	cpl TR0
		
    jnb P4.5, main_game_fix
    ljmp here
    lcall F_n_f
 	
ret

main_game_fix:
jnb P4.5, $
ljmp buffer

Startup:
    mov SP, #7FH
;---------------------------------;
; Hardware initialization         ;
;---------------------------------;
    lcall InitTimer2
    lcall Timer0_Init_0
    lcall LCD_4BIT ; Initialize LCD
    setb EA
    setb P2.0 ; Pin is used as input
    setb P2.1 ; Pin is used as input
    setb P1.1 ; Pin is used as input
    
    MOV score_1,#0
    MOV score_2, #0  ; set both scores initially to 0
    MOV zero, #0
    
    Set_Cursor(1, 1)
	Send_Constant_String(#Welcome)
	Set_Cursor(2, 1)
	Send_Constant_String(#Welcome2)
	
    lcall F_n_f
	
buffer:

    lcall Wait1s
	lcall Wait1s
    
    Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
    
    Set_Cursor(1, 1)
	Send_Constant_String(#high_freq)
	Set_Cursor(2, 1)
	Send_Constant_String(#high_freq_instr)
	
	clr TR0
	mov RH0, #high(TIMER0_RELOAD1)
	mov RL0, #low(TIMER0_RELOAD1)
	setb TR0
	lcall Wait1s
	lcall Wait1s
	cpl TR0
	lcall Wait1s 
	lcall Wait1s
    
    Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
	
	Set_Cursor(1, 1)
	Send_Constant_String(#low_freq)
	Set_Cursor(2, 1)
	Send_Constant_String(#low_freq_instr)
	
	clr TR0
	mov RH0, #high(TIMER0_RELOAD0)
	mov RL0, #low(TIMER0_RELOAD0)
	setb TR0
	lcall Wait1s
	lcall Wait1s
	cpl TR0
	lcall Wait1s 
	lcall Wait1s

    
main_game:
   	
    MOV a, #5
    
    cjne a, score_1, continue
    ljmp Player_1_winner
    
continue:	
    cjne a, score_2, cont2
    ljmp Player_2_winner

cont2: lcall Wait1s

    Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase) 
	Set_Cursor(1, 1)
	Send_Constant_String(#Player1)
	Set_Cursor(1, 10)
	Display_BCD(score_1)
    
    Set_Cursor(2, 1)
	Send_Constant_String(#Player2)
	Set_Cursor(2, 10)
	Display_BCD(score_2) 


;CHOSE THE TONE TO PLAY
tone:
	clr HF
	clr LF
	lcall Random
	mov a , Seed+1
	mov c, acc.3
    mov HLbit, c
	
    jnb P4.5, pause
    sjmp Sound_freq
    
;added pause functionality when push button is pressed
pause:   
    Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
    Set_Cursor(1, 1)
	Send_Constant_String(#Paused) 
	jnb P4.5, $	
	lcall Wait1s
	lcall Wait1s
	lcall Wait1s

    
	ljmp main_game

Sound_freq:
		jb HLbit, not0
	
	
iszero:	
    lcall Wait1s
    lcall Wait_random
	clr TR0
	mov RH0, #high(TIMER0_RELOAD1)
	mov RL0, #low(TIMER0_RELOAD1)
	setb TR0
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#100)
	
	cpl TR0
	Wait_Milli_Seconds(#200)
	setb HF
	ljmp Cap_test 
    
not0:
    lcall Wait1s
	lcall Wait_random
	clr TR0
	mov RH0, #high(TIMER0_RELOAD0)
	mov RL0, #low(TIMER0_RELOAD0)
	setb TR0
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#100) 

	cpl TR0
	Wait_Milli_Seconds(#200)
	
	setb LF
	lcall Cap_test 
	
Cap_test:

    ; Measure the period applied to pin P2.0
    clr TR2 ; Stop counter 2
    mov TL2, #0
    mov TH2, #0
    jb P2.0, $
    jnb P2.0, $
    mov R0, #100
    setb TR2 ; Start counter 0
    
meas_loop1:
    jb P2.0, $
    jnb P2.0, $
    djnz R0, meas_loop1 ; Measure the time of 100 periods
    clr TR2 ; Stop counter 2, TH2-TL2 has the period
    ; save the period of P2.0 for later use
    
    mov x+0, TL2
    mov x+1, TH2
    mov x+2, #0
    mov x+3, #0
    
    Load_y(59000)
    lcall x_lt_y
    jb mf, Player1_touch  
    
    ; Measure the period applied to pin P2.1
    clr TR2 ; Stop counter 2
    mov TL2, #0
    mov TH2, #0
    jb P2.1, $
    jnb P2.1, $
    mov R0, #100
    setb TR2 ; Start counter 0
    
meas_loop2:
    jb P2.1, $
    jnb P2.1, $
    djnz R0, meas_loop2 ; Measure the time of 100 periods
    clr TR2 ; Stop counter 2, TH2-TL2 has the period
    ; save the period of P2.1 for later use
    mov Period_B+0, TL2
    mov Period_B+1, TH2

    mov x+0, TL2
    mov x+1, TH2
    mov x+2, #0
    mov x+3, #0
    
    Load_y(51000)
    lcall x_lt_y
    jb mf, Player2_touch 
    

ljmp main_game

Player1_touch:    
      
      clr mf
      jb HF, Inc_P1
      jb LF, Dec_P1
      
Player2_touch:
	
	 clr mf  
     jb HF, Inc_P2
     jb LF, Dec_P2
     
Inc_P1:

INC score_1
ljmp main_game

Dec_P1:
MOV a, zero
cjne a, score_1, decrement
ljmp main_game

decrement:
DEC score_1
ljmp main_game

Inc_P2:

INC score_2
ljmp main_game

Dec_P2:
MOV a, zero
cjne a,score_2, decrement_2
ljmp main_game

decrement_2:
DEC score_2
ljmp main_game   


Player_1_winner:

    Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)
	
abc:	Set_Cursor(1, 1)
	Send_Constant_String(#Player1)
	Set_Cursor(1, 10)
	Send_Constant_String(#Winner)
	Set_Cursor(2, 1)
	Send_Constant_String(#Player2)
	Set_Cursor(2, 10)
	Send_Constant_String(#Loser)
	lcall song_2
	ljmp abc
    

Player_2_winner:

    Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)

 xyz:   Set_Cursor(2, 1)
	Send_Constant_String(#Player2)
	Set_Cursor(2, 10)
	Send_Constant_String(#Winner)
	Set_Cursor(1, 1)
	Send_Constant_String(#Player1)
	Set_Cursor(1, 10)
	Send_Constant_String(#Loser)
	lcall song_2
	ljmp xyz
	
	
;code for second song - Lucid Dreams by Juice Wrld
song_2:
	clr TR0
	mov RH0, #high(NOTE_A_RELOAD)
	mov RL0, #low(NOTE_A_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	
	cpl TR0 
	Wait_Milli_Seconds(#75)
	
	setb TR0
	
	clr TR0
	mov RH0, #high(NOTE_Db_RELOAD)
	mov RL0, #low(NOTE_Db_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#100)
	
	cpl TR0
	Wait_Milli_Seconds(#55)
	
	setb TR0
	
	clr TR0
	mov RH0, #high(NOTE_A_RELOAD)
	mov RL0, #low(NOTE_A_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	
	cpl TR0
	Wait_Milli_Seconds(#75)
	
	setb TR0
	
	clr TR0
	mov RH0, #high(NOTE_Ab_RELOAD)
	mov RL0, #low(NOTE_Ab_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	
	cpl TR0
	Wait_Milli_Seconds(#75)
	
	setb TR0
	
	clr TR0
	mov RH0, #high(NOTE_Db_RELOAD)
	mov RL0, #low(NOTE_Db_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#100)
	
	cpl TR0
	Wait_Milli_Seconds(#55)
	
	setb TR0
	
	clr TR0
	mov RH0, #high(NOTE_Ab_RELOAD)
	mov RL0, #low(NOTE_Ab_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	
	cpl TR0
	Wait_Milli_Seconds(#75)
	
	setb TR0
	
	clr TR0
	mov RH0, #high(NOTE_Fs_RELOAD)
	mov RL0, #low(NOTE_Fs_RELOAD)
	setb TR0
	
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	
	cpl TR0
	Wait_Milli_Seconds(#75)
		
	setb TR0
	
	clr TR0
	mov RH0, #high(NOTE_B_RELOAD)
	mov RL0, #low(NOTE_B_RELOAD)
	setb TR0
	
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#100)
	
	cpl TR0
	Wait_Milli_Seconds(#55)
	
	setb TR0
	
	clr TR0
	mov RH0, #high(NOTE_Fs_RELOAD)
	mov RL0, #low(NOTE_Fs_RELOAD)
	setb TR0
	
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	
	cpl TR0
	Wait_Milli_Seconds(#75)
	
	setb TR0
	
	clr TR0
	mov RH0, #high(NOTE_B5_RELOAD)
	mov RL0, #low(NOTE_B5_RELOAD)
	setb TR0
	
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#80)
	
	cpl TR0
	Wait_Milli_Seconds(#100)
	
	setb TR0
	
	clr TR0
	mov RH0, #high(NOTE_F_RELOAD)
	mov RL0, #low(NOTE_F_RELOAD)
	setb TR0
	
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#200)
	
	cpl TR0
	Wait_Milli_Seconds(#250)
	
	
	jnb P4.5, piano			;goes to piano when button is pressed, otherwise song plays in loop
    lcall song_2	
ret

;piano that works for two notes A and B. On introducing the dielectric, we hear the respective note
piano:
    Set_Cursor(1, 1)
	Send_Constant_String(#Erase)
	Set_Cursor(2, 1)
	Send_Constant_String(#Erase)

	Set_Cursor(1, 1)
	Send_Constant_String(#piano_message1)
	Set_Cursor(2, 1)
	Send_Constant_String(#piano_message2)
 ; Measure the period applied to pin P2.0
    clr TR2 ; Stop counter 2
    mov TL2, #0
    mov TH2, #0
    jb P2.0, $
    jnb P2.0, $
    mov R0, #100
    setb TR2 ; Start counter 0
    
meas_loop3:

    jb P2.0, $
    jnb P2.0, $
    djnz R0, meas_loop3 ; Measure the time of 100 periods
    clr TR2 ; Stop counter 2, TH2-TL2 has the period
    ; save the period of P2.0 for later use
    
    mov x+0, TL2
    mov x+1, TH2
    mov x+2, #0
    mov x+3, #0
    
    Load_y(59000)
    lcall x_lt_y
    jb mf, note1  
    
    ; Measure the period applied to pin P2.1
    clr TR2 ; Stop counter 2
    mov TL2, #0
    mov TH2, #0
    jb P2.1, $
    jnb P2.1, $
    mov R0, #100
    setb TR2 ; Start counter 0
    
meas_loop4:
    jb P2.1, $
    jnb P2.1, $
    djnz R0, meas_loop4 ; Measure the time of 100 periods
    clr TR2 ; Stop counter 2, TH2-TL2 has the period
    ; save the period of P2.1 for later use
    mov Period_B+0, TL2
    mov Period_B+1, TH2

    mov x+0, TL2
    mov x+1, TH2
    mov x+2, #0
    mov x+3, #0
    
    Load_y(51000)
    lcall x_lt_y
    jb mf, note2 
    
    ljmp piano

note1:
	
	clr TR0
	mov RH0, #high(NOTE_A_RELOAD)
	mov RL0, #low(NOTE_A_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#200)
	cpl TR0
	Wait_Milli_Seconds(#50)
	ljmp piano

note2:
	clr TR0
	mov RH0, #high(NOTE_B_RELOAD)
	mov RL0, #low(NOTE_B_RELOAD)
	setb TR0
	Wait_Milli_Seconds(#200)
	cpl TR0
	Wait_Milli_Seconds(#50)
	ljmp piano	
	
END