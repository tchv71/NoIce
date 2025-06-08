;  MONZ80.asm - Z80 Debug monitor for use with NoICEZ80
;  This file may be assembled with the Alfred Arnold as assembler
;
;  Copyright (c) 2018 by John Hartman
;
;  Modification History:
;       14-Jun-93 JLH release version
;       24-Aug-93 JLH bad constant for COMBUF length compare
;        8-Nov-93 JLH change to use RST 8 for breakpoint, not RST 38 (v1.3)
;       20-Dec-93 JLH bug in NMI dummy vectors: overwrote NMI and reset!
;       17-Oct-95 JLH eliminate two-arg for SUB, AND, OR, XOR, and CP
;       21-Jul-00 JLH change FN_MIN from F7 to F0
;       12-Mar-01 JLH V3.0: improve text about paging, formerly called "mapping"
;       11-Jan-05 JLH V3.1: correct bug in Z180 reset/illegal op-code trap
;       12-Nov-18 JLH modify to assemble with Arnold's as
;
;============================================================================
;
; To assemble, assuming that the Arnold "asw.exe" and "p2hex.exe" are on your path,
;    asw MonZ80.asm -L -E
;       Where
;       -L           generate a listing file MonZ80.LST
;       -E           generate an error.log file (errors are also shown in the listing)
;    p2hex MonZ80.p MonZ80.ihx -F Intel -r $0000-$ffff -l 32
;       The file "MonZ80.ihx" will contain the Intel hex output.
;       The file "MonZ80.lst" will contain an assembly listing
;
;  To customize for a given target, you must change code in the
;  hardware equates, the string TSTG, and the routines RESET and REWDT.
;  You may or may not need to change GETCHAR, PUTCHAR, depending on
;  how peculiar your UART is.
;
;  For more information, refer to the NoICE help file monitor.htm
;
;  To add banked or paged memory support:
;  1) Define page latch port PAGELATCH here
;  2) If PAGELATCH is write only, define or import the latch port's RAM
;     image PAGEIMAGE here (The application code must update PAGEIMAGE
;     before outputing to PAGELATCH)
;  3) Search for and modify PAGELATCH, PAGEIMAGE, and REG_PAGE usage below
;  4) In TSTG below edit "LOW AND HIGH LIMIT OF PAGED MEM"
;     to appropriate range (typically 4000H to 07FFFH for two-bit MMU)
;
;  For more information, refer to the NoICE help file 2bitmmu.htm
;
;  This file contains conditional assemblies for several options:
;  1) Z180 and peripherals.  (Z180 equ 1)
;     This will run unmodified on the Zilog Z8018000ZC0 Z180/SCC
;     evaluation board using SCC-B for host communications
;  2) Z84C15 and peripherals.  (Z84C15 equ 1)
;     This will run unmodified on the Zilog Z84C1500ZC0 Z84C15
;     evaluation board using SIO-B for host communications
;  3) Z80 and custom peripherals.  (Z80 equ 1)
;     You will need to define equates and initialization code to match your
;     UART and other preipherals.
;
;  I/O EQUATES for Z80, Z180, or Z84C15.
;  Set exactly one true.  "Z80" will need additional equates to match your hardware
Z80	equ     1
    .Z80

;
;  If your taget differs from these options, you may need to make other
;  modifications to this file to reflect your processor and hardware.
;
;============================================================================
;
USE_PORT        EQU     1
CPM             EQU     1
;  Hardware definitions
ROM_START	equ 8800h;0C800h		; START OF MONITOR CODE
RAM_START	equ 8c80h;0CC80h		; 0d310h            ;START OF MONITOR RAM
USER_CODE	equ 8700h;0C700h			; RST 1 handler
?CTRL	equ	40h
?SHIFT	equ	20h
IFDEF  USE_PORT

FT245R  equ     0D0h    ;  base address of FT245R FIFO
DATA    equ     0       ;  Data register
STAT    equ     1       ;  Status register
;
;  Define FIFO port
FIFO_DATA   equ   FT245R+DATA
FIFO_STATUS equ   FT245R+STAT

RXEMPTY equ     1       ; MASK FOR RX BUFFER EMPTY
TXFULL  equ     2       ; MASK FOR TX BUFFER FULL

ELSE
;
;  Equates for I/O mapped 8250 or 16450 serial port
VV55	equ     50H			; 0C400h   ;base of 16450 UART
USER_PORT	equ VV55
DATA    equ     0       ;  Data register
STAT	equ     2			; Status register
;
;  Define monitor serial port
SERIAL_STATUS	equ   VV55+STAT
CLIENT_STATUS	equ   VV55+1
SERIAL_DATA	equ   VV55+DATA
SERIAL_CONTROL	equ  VV55+3

RXRDY	equ           1			; MASK FOR RX BUFFER FULL
TXRDY	equ           2			; MASK FOR TX BUFFER EMPTY
;
;  op-code equates for IN and OUT
;OP_IN   equ     0DBh
;OP_OUT  equ     0D3h
;OP_RET  equ     0C9h
ENDIF
        LD      HL,START_CODE
        LD      DE,ROM_START
        LD      BC,INIOUT-ROM_START
        LDIR

        LD      HL,(1)
        PUSH    HL
        LD      HL,(6)
        PUSH     HL
        LD      HL,INT_VEC+8;INT_VEC
        LD      DE,R0+8
        LD      BC, 0Eh-8;INT_VEC_END-R0
        LDIR

        POP     HL
        LD      (6),HL
        POP     HL
        LD      (1),HL
        LD      A,0C3H
        LD      (0),A
        LD      (5),A
        JP	RESET


;============================================================================
;  RAM definitions:  top 1K
        .PHASE     RAM_START               ; Monitor RAM
;
;  Initial user stack
;  (Size and location is user option)
        DS      64
INITSTACK:
;
;  Monitor stack
;  (Calculated use is at most 6 bytes.  Leave plenty of spare)
        DS      16
MONSTACK:
;
;  Target registers:  order must match that in TRGZ80.C
TASK_REGS:
 REG_STATE:     DS      1
 REG_PAGE:      DS      1
 REG_SP:        DS      2
 REG_IX:        DS      2
 REG_IY:        DS      2
 REG_HL:        DS      2
 REG_BC:        DS      2
 REG_DE:        DS      2
 REG_AF:                        ;LABEL ON FLAGS, A AS A WORD
 REG_FLAGS:     DS      1
 REG_A:         DS      1
 REG_PC:        DS      2
 REG_I:         DS      1
 REG_IFF:       DS      1
 ;
 REG_HLX:       DS      2       ;ALTERNATE REGISTER SET
 REG_BCX:       DS      2
 REG_DEX:       DS      2
 REG_AFX:                       ;LABEL ON FLAGS, A AS A WORD
 REG_FLAGSX:    DS      1
 REG_AX:        DS      1
TASK_REGS_SIZE  EQU     $-TASK_REGS
; !!! Caution:  don't put parenthesis around the above in ASM180:
; !!! The parenthesis in ($-TASK_REGS) are "remembered", such that
; !!! LD BC,TASK_REGS_SIZE is the same as LD BC,(TASK_REGS_SIZE)
; !!! It is OK to use parenthesis around the difference if the difference
; !!! is to be divided - just not around the entire expression!!!!!
;
;  Communications buffer
;  (Must be at least as long as TASK_REG_SIZE.  Larger values may improve
;  speed of NoICE memory load and dump commands)
COMBUF_SIZE     EQU     67              ;DATA SIZE FOR COMM BUFFER
COMBUF:         DS      2+COMBUF_SIZE+1 ;BUFFER ALSO HAS FN, LEN, AND CHECK
;
RAM_END         EQU     $               ;ADDRESS OF TOP+1 OF RAM
;
       .dephase
;===========================================================================
;  8080 mode Interrupt vectors
;
;  Reset, RST 0,  or trap vector
INT_VEC:
        .phase 0 ;ORG     0H
R0:     JP      RESET
        NOP
        NOP
        NOP
        NOP
        NOP
;
;  Interrupt RST 08.  Used for breakpoint.  Any other RST
;  may be used instead by changing the code below and the value of the
;  breakpoint instruction in the status string TSTG.  If RST NN cannot
;  be used, then CALL may be used instead.  However, this will restrict
;  the placement of breakpoints, since CALL is a three byte instruciton.
        PUSH    AF
        LD      A,1                     ;STATE = 1 (BREAKPOINT)
        JP      INT_ENTRY
        NOP
        NOP
;
;  Interrupt RST 10
        JP      USER_CODE + 10H
        NOP
        NOP
        NOP
        NOP
        NOP
;
;  Interrupt RST 18
        JP      USER_CODE + 18H
        NOP
        NOP
        NOP
        NOP
        NOP
;
;  Interrupt RST 20
        JP      USER_CODE + 20H
        NOP
        NOP
        NOP
        NOP
        NOP
;
;  Interrupt RST 28
        JP      USER_CODE + 28H
        NOP
        NOP
        NOP
        NOP
        NOP
;
;  Interrupt RST 30
        JP      USER_CODE + 30H
        NOP
        NOP
        NOP
        NOP
        NOP
;
;  Interrupt RST 38
        JP      USER_CODE + 38H
        NOP
        NOP
        NOP
        NOP
        NOP
;
;===========================================================================
;
;  Non-maskable interrupt:  bash button
;  PC is stacked, interrupts disabled, and IFF2 has pre-NMI interrupt state
;
;  At the user's option, this may vector thru user RAM at USER_CODE+66H,
;  or enter the monitor directly.  This will depend on whether or not
;  the user wishes to use NMI in the application, or to use it with
;  a push button to break into running code.
        DS      66H-$ ;ORG     R0+66H
NMI_ENTRY:
        PUSH    AF
        LD      A,2
        JP      INT_ENTRY
;
;  Or, if user wants control of NMI:
;;      JP      USER_CODE + 66H         ;JUMP THRU VECTOR IN RAM
;;  (and enable NMI handler in DUMMY_INTS below)
;
;===========================================================================
;
;  Dummy handlers for RST and NMI.  This code is moved to the beginning
;  of USER_RAM, where the RST and NMI interrupts jump to it.  The code
;  then enters the monitor, specifying a STATE value which identifies the
;  interrupt which occurred.  This facilitates identification of
;  unexpected interrupts.  If the user desires, s/he may overwrite the
;  beginning of USER_RAM with appropriate handler code.  Spacing of
;  this code is designed such that the user may re-ORG to 0 to run
;  the code from ROM.
INT_VEC_END:
        .DEPHASE
;
START_CODE:
        ;CSEG
        .PHASE ROM_START;ORG     ROM_START
        JP	R0
;
;===========================================================================
;
;  Dummy handler for RST 1.  This code is copied to USER_RAM, where the
;  RST 1 interrupt calls it.  The code then enters the monitor, specifying
;  a STATE value which identifies the interrupt which occurred.
;
;  RST 1 is used for breakpoint.  Any other RST may be used instead by
;  changing the code below and the value of the breakpoint instruction
;  in the status string TSTG.  If RST NN cannot be used, then CALL may be
;  used instead. However, this will restrict the placement of breakpoints,
;  since CALL is a three byte instruciton.
;
DUMMY_INTS:
;
;  RST 0
        PUSH    AF
        LD      A,0                     ;STATE = 0 (INTERRUPT 0)
        JP      INT_ENTRY
        NOP
        NOP
;
;  (vectored only if not used for breakpoint)
;  RST 8
        PUSH    AF
        LD      A,3                     ;STATE = 3 (INTERRUPT 8)
        JP      INT_ENTRY
        NOP
        NOP
;
;  RST 10H
        PUSH    AF
        LD      A,4                     ;STATE = 4 (INTERRUPT 10)
        JP      INT_ENTRY
        NOP
        NOP
;
;  RST 18H
        PUSH    AF
        LD      A,5                     ;STATE = 5 (INTERRUPT 18)
        JP      INT_ENTRY
        NOP
        NOP
;
;  RST 20H
        PUSH    AF
        LD      A,6                     ;STATE = 6 (INTERRUPT 20)
        JP      INT_ENTRY
        NOP
        NOP
;
;  RST 28H
        PUSH    AF
        LD      A,7                     ;STATE = 7 (INTERRUPT 28)
        JP      INT_ENTRY
        NOP
        NOP
;
;  RST 30H
        PUSH    AF
        LD      A,8                     ;STATE = 8 (INTERRUPT 30)
        JP      INT_ENTRY
        NOP
        NOP
;
;  RST 38H
        PUSH    AF
        LD      A,9                     ;STATE = 9 (INTERRUPT 38)
        JP      INT_ENTRY
;
;  Use this if NMI is to be vectored through RAM.  Else comment it out
;  to have NMI pass directy to monitor to break into running programs.
;;;     ORG     DUMMY_INTS+66H
;;;     PUSH    AF
;;;     LD      A,2
;;;     JP      INT_ENTRY
DUMMY_SIZE	equ $-DUMMY_INTS

@SYSREG	MACRO	VAL
	IN	A,(-1)
	LD	A,VAL
	OUT	(-1),A
	ENDM
; IN and OUT MACRO comands
@IN	MACRO	addr
IF ((addr) LT 256)
	IN	A,(addr)
ELSE
	LD	A,(addr)
ENDIF
	ENDM

@OUT	MACRO	addr
IF ((addr) LT 256)
	OUT	(addr),A
ELSE
	LD	(addr),A
ENDIF
	ENDM

;
;===========================================================================
;  Power on reset
RESET:
IFDEF   CPM
	DI
	LD	DE,VRAM_ADR
	CALL	BIOS_CONFIG
ENDIF
IFDEF  USE_PORT
	@SYSREG 0C0h
	LD      A,12
	out     (FT245R),A
	out     (FT245R + 1), A
	@SYSREG 80h
ENDIF
	LD	HL,Prompt
IFNDEF  CPM
        CALL    0F818h
ELSE
        CALL    PUTS
ENDIF
IFNDEF USE_PORT
	LD	A,9
	CALL	StartCommand
ENDIF
	JP	Init
BIOS_CONFIG:
	LD	HL,(1)
	LD	BC,30H
	add	hl,bc
	LD	A,(hl)
	CP	0C3h ;JMP
	ret	nz
	inc	hl
	push	de
	LD	E,(hl)
	inc	hl
	LD	D,(hl)
	ex	de,hl
	pop	de
	jp	(hl)

_in::	INC	H
	DEC	H
	JP	Z,_in_port
	LD	A,(hl)
	ret
_in_port:
	LD	A,L
	LD	($+4),A
	IN	A,(0)
	ret

_out::	INC	H
	DEC	H
	JP	Z,_out_port
	LD	(HL),A
	ret
_out_port:
	push	af
	LD	A,L
	LD	($+5),A
	pop	af
	OUT	(0),A
	ret

PUTS:	LD	A,(hl)
	inc	hl
	LD	C,A
	OR	A
	ret	z
	CALL	CONOUT
	JP	PUTS

CONOUT: push	hl
	CALL	CONOU1
	pop	hl
	ret

CONOU1:
	LD	HL,(1)
	push	de
	LD	DE,9
	add	hl,de
	LD	A,(hl)
	CP	0C3h ; JMP
	pop	de
	ret	nz
	inc	hl
	push	de
	LD	E,(hl)
	inc	hl
	LD	D,(hl)
	ex	de,hl
	pop	de
	jp	(hl)


IF 0
; A - value
; C - port number
; B - count
SETPORT:
	LD	(HL),C
BEGPRO:
	OUT	(0),A
	INC	(hl)
	DEC	B
	ret	z
	JP	BEGPRO
ENDIF

Prompt:     DB	"NOICE Z80 MONITOR V3.12 (CTRL+SHIFT TO BREAK)",0

IFNDEF USE_PORT
SEND_MODE	equ 10000000b		; Режим передачи (1 0 0 A СH 0 B CL)
RECV_MODE	equ 10010000b		; Режим приема (1 0 0 A СH 0 B CL)

ERR_START   	equ 040h
ERR_WAIT    	equ 041h
ERR_OK_NEXT 	equ 042h
ERR_OK	equ 043h
ERR_OK_READ	equ 044h
ERR_OK_ENTRY	equ 045h
ERR_OK_WRITE	equ 046h
ERR_OK_ADDR  	equ 047h
ERR_OK_BLOCK	equ 04Fh
 ;----------------------------------------------------------------------------
; A start of any command. 
; A - command code

StartCommand:
; The first step is synchronization with the controller
; 256 attempts are performed, each skipping 256+ bytes
; That is, this is the maximum amount of data that the controller can transmit.
     push	bc
     push	hl
     push	af
     LD	C, 0

StartCommand1:
     ; Receive mode (release the bus - port A) and initialize HL
     CALL	SwitchRecv

     ; Beginning of any command (play a sequence in address bus)
     ;LXI	H, USER_PORT+1
     LD	A, 0
     @OUT	USER_PORT+1
     LD	A, 44h
     @OUT	USER_PORT+1
     LD	A, 40h
     @OUT	USER_PORT+1
     LD	A, 0h
     @OUT	USER_PORT+1

     ; If there is synchronization, then the controller will respond with ERR_START
     CALL	Recv
     CP	ERR_START
     JP	Z,StartCommand2

     ; Pause. And also we skip 256 bytes (in total it will be
     ; 64 KB data skipped, maximum packet size)
     push	bc
     LD	C, 0
StartCommand3:
     CALL	Recv
     DEC	C
     JP	NZ,StartCommand3
     pop	bc

     ; Попытки
     DEC	C
     JP	NZ,StartCommand1

     ; Код ошибки
     LD	A, ERR_START
StartCommandErr2:
     pop	bc
     pop	hl
     pop	bc
     ;POP	B
     ret

;----------------------------------------------------------------------------
; Synchronization with the controller is done. The controller should respond with ERR_OK_NEXT

StartCommand2:
     ; Ответ         	
     CALL	WaitForReady
     CP	ERR_OK_NEXT
     JP	NZ,StartCommandErr2

     ; Переключаемся в режим передачи
     CALL	SwitchSend

     pop	af
     pop	hl
     pop	bc

     ; Передаем код команды
     JP	Send2

;----------------------------------------------------------------------------
; Switch to send mode

SwitchSend:
     CALL	Recv
SwitchSend0:
     LD	A, SEND_MODE
     @OUT	USER_PORT+3
     ret

;----------------------------------------------------------------------------
; Successful end of the command
; and an additional cycle so that the microcontroller releases the bus
Ret0:
     XOR	A

;----------------------------------------------------------------------------
; Command ending with an error in A
; and an additional cycle so that the microcontroller releases the bus
EndCommand:
     push	af
     CALL	Recv
     pop	af
     ret

;----------------------------------------------------------------------------
; Switch to receive mode

SwitchRecv:
     LD	A, RECV_MODE
     @OUT	USER_PORT+3
     ret					; ----------------------------------------------------------------------------
;Switch to receive mode and wait for microcontroller be ready

SwitchRecvAndWait:
     CALL	SwitchRecv

;----------------------------------------------------------------------------
WaitForReady:
     CALL	Recv
     CP	ERR_WAIT
     JP	Z,WaitForReady
     ret


;----------------------------------------------------------------------------
; Send a byte from A.

Send2:
     @OUT	USER_PORT

;----------------------------------------------------------------------------
; Receive a byte into А

Recv:
     LD	A, 20h
     @OUT	USER_PORT+1
     XOR	A
     @OUT	USER_PORT+1
     @IN	USER_PORT
     ret					;
ENDIF
;-------------------------------------------------------------------------
;  Initialize monitor
INIT:   LD	SP,MONSTACK
;
;  Initialize target hardware
        LD      HL,INIOUT       ;PUT ADRESS OF INITIALIZATION DATA TABLE INTO HL
        LD      D,OUTCNT        ;PUT NUMBER OF DATA AND ADDR. PAIRS INTO REG. B
;
;  Caution:  OUT and OUTI place the 8 bit address from C on A7-A0, but
;  the contents of the B register on A15-A7.  The Z180's on-chip peripherals
;  decode 16 bits of I/O address, for reasons known only to ZIlog.
;  Thus, either be sure B=0 or use the Z180 OTIM
;  We do the former, so as to operate the same code on Z84C15 or Z80
        LD      B,0             ;so a15-a8 will be 0
        LD      A,D
        OR      A
        JR      Z,NOINIT
RST10:  LD      C,(HL)          ;load address from table
        INC     HL
        LD      A,(HL)          ;load data from table
        INC     HL
        OUT     (C),A           ;output a to i/o address (A15-A8 = 0)
        DEC     D
        JR      NZ,RST10        ;loop for d (address, data) pairs
NOINIT:
;
;===========================================================================
;  Perform user hardware initilaization here

;===========================================================================
;  Initialize user interrupt vectors to point to monitor
        LD	HL,DUMMY_INTS		; dummy handler code
        LD	DE,USER_CODE		; start of user codespace
        LD	BC,DUMMY_SIZE		; number of bytes
        LDIR                            ;copy code

;===========================================================================
;
;  Initialize user registers
        LD	HL,INITSTACK
        LD	(REG_SP),HL		; INIT USER'S STACK POINTER
        XOR	A
        LD	H,A
        LD	L,A
        LD      (REG_HL),HL
        LD      (REG_BC),HL
        LD      (REG_DE),HL
        LD      (REG_IX),HL
        LD      (REG_IY),HL
        LD      (REG_AF),HL
        LD      (REG_HLX),HL
        LD      (REG_BCX),HL
        LD      (REG_DEX),HL
        LD      (REG_AFX),HL
        LD      (REG_I),A
        LD      (REG_STATE),A           ;set state as "RESET"
        LD      (REG_IFF),A             ;NO INTERRUPTS
;
;  Initialize memory paging variables and hardware (if any)
        LD      (REG_PAGE),A            ;page 0
;;;     LD      (PAGEIMAGE),A
;;;     OUT     (PAGELATCH),A           ;set hardware page
;
;  Set function code for "GO".  Then if we reset after being told to
;  GO, we will come back with registers so user can see the crash
        LD	A,FN_RUN_TARGET
        LD	(COMBUF),A
        JP	RETURN_REGS		; DUMP REGS, ENTER MONITOR
;
;===========================================================================
;  Get a character to A
;
;  Return A=char, CY=0 if data received
;         CY=1 if timeout (0.5 seconds)
;
;  Uses 6 bytes of stack including return address
;
GETCHAR:
IFDEF  USE_PORT
        PUSH    DE
        LD      DE,8000h      ;long timeout
gc10:   DEC     DE
        LD      A,D
        OR      E
        JR      Z, gc90       ;exit if timeout
        @in     FIFO_STATUS   ;read device status
        AND     RXEMPTY
        JR      NZ, gc10          ;not ready yet.
;
;  Data received:  return CY=0. data in A
        XOR     A             ;cy=0
        @in     FIFO_DATA     ;read data
        ;PUSH    PSW
        ;CALL    0F815h
        ;POP     PSW
        POP     DE
        RET
;
;  Timeout:  return CY=1
gc90:   SCF                   ;cy=1
        POP     DE
        RET
ELSE
        LD	A,99H			; A - input, B - output, Clow, Chigh - input
        @OUT	SERIAL_CONTROL
        LD	A,1
        @OUT	CLIENT_STATUS
        push	de
        LD	DE,8000h			; long timeout
gc10:   DEC	bc
        LD	A,D
        OR	E
        JR	Z,gc90			; exit if timeout
        @IN	SERIAL_STATUS		; read device status
        AND	RXRDY
        JR	NZ,gc10			; not ready yet.
;
;  Data received:  return CY=0. data in A
        XOR	A			; cy=0
        @IN	SERIAL_DATA		; read data
        push	af
        XOR	a
        @OUT	CLIENT_STATUS
gc11:   @IN	SERIAL_STATUS		; wait for server
        AND	RXRDY
        JP	Z,gc11
        pop	af
        pop	de
        ret
;
;  Timeout:  return CY=1
gc90:   scf				; cy=1
        LD	A,0
        @OUT	CLIENT_STATUS
        pop	de
        ret
ENDIF
;
;===========================================================================
;  Output character in A
;
;  Uses 6 bytes of stack including return address
;
PUTCHAR:
IFDEF  USE_PORT
        PUSH    AF            ;save byte to output
pc10:   @in     FIFO_STATUS   ;read device status
        AND     TXFULL        ;tx full ?
        JR      NZ, pc10
        POP     AF
        @out    FIFO_DATA     ;transmit char
        RET
ELSE
        push	af			; save byte to output
        LD	A,89H			; A - output, B - output, Clow, Chigh - input
        @OUT	SERIAL_CONTROL
        LD	A,2
        @OUT	CLIENT_STATUS		; // Ready to send
pc10:   CALL	CheckBrk
        JP	Z,0
        @IN	SERIAL_STATUS		; read device status
        AND	TXRDY			; rx ready ?
        JR	NZ,pc10

        pop	af
        @OUT	SERIAL_DATA		; transmit char - error in wiring! must write to _STATUS
        XOR	a
        @OUT	CLIENT_STATUS

pc11::	CALL    CheckBrk
        JP	Z,0
        @IN	SERIAL_STATUS		; wait for server confirms reading a byte
        AND	TXRDY
        JR	Z,pc11
        ret
ENDIF

CheckBrk:
        push	hl
        LD	HL,(PPI_ADR)
        inc	hl
        inc	hl
        CALL	_in
        pop	hl
        AND	?Ctrl+?Shift
        ret
;
;===========================================================================
;  Response string for GET TARGET STATUS request
;  Reply describes target:
TSTG:   DB      0                       ;2: PROCESSOR TYPE = Z80
        DB      COMBUF_SIZE             ;3: SIZE OF COMMUNICATIONS BUFFER
        DB      0                       ;4: NO OPTIONS
        DW      0                       ;5,6: BOTTOM OF PAGED MEM (none)
        DW      0                       ;7,8: TOP OF PAGED MEM (none)
        DB      B1-B0                   ;9 BREAKPOINT INSTRUCTION LENGTH
B0:     RST     08H                     ;10+ BREAKPOINT INSTRUCTION
B1:
        DB      "NoICE Z80 monitor V3.1",0              ;DESCRIPTION, ZERO
TSTG_SIZE       EQU     $-TSTG          ;SIZE OF STRING
;
;===========================================================================
;  HARDWARE PLATFORM INDEPENDENT EQUATES AND CODE
;
;  Communications function codes.
FN_GET_STATUS	equ 0FFh			; reply with device info
FN_READ_MEM	equ 0FEh			; reply with data
FN_WRITE_MEM	equ 0FDh			; reply with status (+/-)
FN_READ_REGS	equ 0FCh			; reply with registers
FN_WRITE_REGS	equ 0FBh			; reply with status
FN_RUN_TARGET	equ 0FAh			; reply (delayed) with registers
FN_SET_BYTES	equ 0F9h			; reply with data (truncate if error)
FN_IN	equ 0F8h				; input from port
FN_OUT	equ 0F7h				; output to port
;
FN_MIN	equ 0F0h				; MINIMUM RECOGNIZED FUNCTION CODE
FN_ERROR	equ 0F0h			; error reply to unknown op-code
;
;===========================================================================
;  Enter here via RST nn for breakpoint:  AF, PC are stacked.
;  Enter with A=interrupt code = processor state
;  Interrupt status is not changed from user program
INT_ENTRY:
;
;  Interrupts may be on:  get IFF as quickly as possible, so we can DI
        LD      (REG_STATE),A   ;save entry state
        LD      (REG_HL),HL     ;SAVE HL
        LD      A,I             ;GET P FLAG = IFF2 (SIDE EFFECT)
        DI                      ;NO INTERRUPTS ALLOWED
;
        LD      (REG_I),A       ;SAVE INT REG
        LD      A,0
        JP      PO,BREAK10      ;JIF PARITY ODD (FLAG=0)
        INC     A               ;ELSE SET A = FLAG = 1 (ENABLED)
BREAK10: LD     (REG_IFF),A     ;SAVE INTERRUPT FLAG
;
;  Save registers in reg block for return to master
        POP     HL              ;GET FLAGS IN L, ACCUM IN H
        LD      (REG_AF),HL     ;SAVE A AND FLAGS
;
;  If entry here was by breakpoint (state=1), then back up the program
;  counter to point at the breakpoint/RST instruction.  Else leave PC alone.
;  (If CALL is used for breakpoint, then back up by 3 bytes)
        POP     HL              ;GET PC OF BREAKPOINT/INTERRUPT
        LD      A,(REG_STATE)
        CP      1
        JR      NZ,NOTBP        ;JIF NOT A BREAKPOINT
        DEC     HL              ;BACK UP PC TO POINT AT BREAKPOINT
NOTBP:  JP      ENTER_MON       ;HL POINTS AT BREAKPOINT OPCODE
;
;===========================================================================
;  Main loop:  wait for command frame from master
MAIN:   CALL	CheckBrk
IFDEF CPM
        JP	Z,0
ELSE
        JP      Z,0F86Ch
ENDIF

        LD	SP,MONSTACK		; CLEAN STACK IS HAPPY STACK
        LD	HL,COMBUF		; BUILD MESSAGE HERE
;
;  First byte is a function code
        CALL	GETCHAR			; GET A FUNCTION (uses 6 bytes of stack)
        JR      C,MAIN                  ;JIF TIMEOUT: RESYNC
        CP      FN_MIN
        JR      C,MAIN                  ;JIF BELOW MIN: ILLEGAL FUNCTION
        LD	(HL),A			; SAVE FUNCTION CODE
        inc	hl
;
;  Second byte is data byte count (may be zero)
        CALL	GETCHAR			; GET A LENGTH BYTE
        JR      C,MAIN                  ;JIF TIMEOUT: RESYNC
        CP      COMBUF_SIZE+1
        JR      NC,MAIN                 ;JIF TOO LONG: ILLEGAL LENGTH
        LD      (HL),A                  ;SAVE LENGTH
        INC     HL
        OR      A
        JR      Z,MA80                  ;SKIP DATA LOOP IF LENGTH = 0
;
;  Loop for data
        LD      B,A                     ;SAVE LENGTH FOR LOOP
MA10:   CALL    GETCHAR                 ;GET A DATA BYTE
        JR      C,MAIN                  ;JIF TIMEOUT: RESYNC
        LD      (HL),A                  ;SAVE DATA BYTE
        INC     HL
        DJNZ    MA10
;
;  Get the checksum
MA80:   CALL	GETCHAR			; GET THE CHECKSUM
        JR      C,MAIN                  ;JIF TIMEOUT: RESYNC
        LD	C,A			; SAVE CHECKSUM
;
;  Compare received checksum to that calculated on received buffer
;  (Sum should be 0)
        CALL	CHECKSUM
        ADD	A,C
        JR      NZ,MAIN                 ;JIF BAD CHECKSUM
;
;  Process the message.
        LD	A,(COMBUF+0)		; GET THE FUNCTION CODE
        CP	FN_GET_STATUS
        JP	Z,TARGET_STATUS
        CP	FN_READ_MEM
        JP	Z,READ_MEM
        CP	FN_WRITE_MEM
        JP	Z,WRITE_MEM
        CP	FN_READ_REGS
        JP	Z,READ_REGS
        CP	FN_WRITE_REGS
        JP	Z,WRITE_REGS
        CP	FN_RUN_TARGET
        JP	Z,RUN_TARGET
        CP	FN_SET_BYTES
        JP	Z,SET_BYTES
        CP	FN_IN
        JP	Z,IN_PORT
        CP	FN_OUT
        JP	Z,OUT_PORT
;
;  Error: unknown function.  Complain
        LD	A,FN_ERROR
        LD	(COMBUF+0),A		; SET FUNCTION AS "ERROR"
        LD	A,1
        JP	SEND_STATUS		; VALUE IS "ERROR"

;===========================================================================
;
;  Target Status:  FN, len
;
TARGET_STATUS:
;
        LD      HL,TSTG                 ;DATA FOR REPLY
        LD      DE,COMBUF+1             ;RETURN BUFFER
        LD      BC,TSTG_SIZE            ;LENGTH OF REPLY
        LD      A,C
        LD      (DE),A                  ;SET SIZE IN REPLY BUFFER
        INC     DE
        LDIR                            ;MOVE REPLY DATA TO BUFFER
;
;  Compute checksum on buffer, and send to master, then return
        JP	SEND

;===========================================================================
;
;  Read Memory:  FN, len, page, Alo, Ahi, Nbytes
;
READ_MEM:
;
;  Set page
;;      LD      A,(COMBUF+2)
;;      LD      (PAGEIMAGE),A
;;      LD      BC,PAGELATCH
;;      OUT     (BC),A
;
;  Get address
        LD	HL,(COMBUF+3)
        LD	A,(COMBUF+5)		; NUMBER OF BYTES TO GET
;
;  Prepare return buffer: FN (unchanged), LEN, DATA
        LD	DE,COMBUF+1		; POINTER TO LEN, DATA
        LD      (DE),A                  ;RETURN LENGTH = REQUESTED DATA
        inc	de
        OR	A
        JR      Z,GLP90                 ;JIF NO BYTES TO GET
;
;  Read the requested bytes from local memory
        LD	B,A
GLP:    LD      A,(HL)                  ;GET BYTE TO A
        LD      (DE),A                  ;STORE TO RETURN BUFFER
        inc	hl
        inc	de
        DJNZ    GLP
;
;  Compute checksum on buffer, and send to master, then return
GLP90:  JP	SEND

;===========================================================================
;
;  Write Memory:  FN, len, page, Alo, Ahi, (len-3 bytes of Data)
;
;  Uses 2 bytes of stack
;
WRITE_MEM:
;
;  Set page
;;      LD      A,(COMBUF+2)
;;      LD      (PAGEIMAGE),A
;;      LD      BC,PAGELATCH
;;      OUT     (BC),A
;
        LD      HL,COMBUF+5             ;POINTER TO SOURCE DATA IN MESSAGE
        LD      DE,(COMBUF+3)           ;POINTER TO DESTINATION
        LD      A,(COMBUF+1)            ;NUMBER OF BYTES IN MESSAGE
        SUB     3                       ;LESS PAGE, ADDRLO, ADDRHI
        JR      Z,WLP50                 ;EXIT IF NONE REQUESTED

;  Write the specified bytes to local memory
        LD      B,A
        PUSH    BC                      ;SAVE BYTE COUNTER
WLP10:  LD      A,(HL)                  ;BYTE FROM HOST
        LD      (DE),A                  ;WRITE TO TARGET RAM
        INC     HL
        INC     DE
        DJNZ    WLP10
;
;  Compare to see if the write worked
        LD      HL,COMBUF+5             ;POINTER TO SOURCE DATA IN MESSAGE
        LD      DE,(COMBUF+3)           ;POINTER TO DESTINATION
        POP     BC                      ;SIZE AGAIN
;
;  Compare the specified bytes to local memory
WLP20:  LD      A,(DE)                  ;READ BACK WHAT WE WROTE
        CP      (HL)                    ;COMPARE TO HOST DATA
        JR      NZ,WLP80                ;JIF WRITE FAILED
        INC     HL
        INC     DE
        DJNZ    WLP20
;
;  Write succeeded:  return status = 0
WLP50:  XOR     A                       ;RETURN STATUS = 0
        JR      WLP90
;
;  Write failed:  return status = 1
WLP80:  LD	A,1
;
;  Return OK status
WLP90:  JP	SEND_STATUS

;===========================================================================
;
;  Read registers:  FN, len=0
;
READ_REGS:
;
;  Enter here from int after "RUN" and "STEP" to return task registers
RETURN_REGS:
        LD      HL,TASK_REGS            ;REGISTER LIVE HERE
        LD      A,TASK_REGS_SIZE        ;NUMBER OF BYTES
;
;  Prepare return buffer: FN (unchanged), LEN, DATA
        LD      DE,COMBUF+1             ;POINTER TO LEN, DATA
        LD      (DE),A                  ;SAVE DATA LENGTH
        INC     DE
;
;  Copy the registers
        LD      B,A
GRLP:   LD      A,(HL)                  ;GET BYTE TO A
        LD      (DE),A                  ;STORE TO RETURN BUFFER
        INC     HL
        INC     DE
        DJNZ    GRLP
;
;  Compute checksum on buffer, and send to master, then return
        JP	SEND

;===========================================================================
;
;  Write registers:  FN, len, (register image)
;
WRITE_REGS:
;
        LD      HL,COMBUF+2             ;POINTER TO DATA
        LD      A,(COMBUF+1)            ;NUMBER OF BYTES
        OR      A
        JR      Z,WRR80                 ;JIF NO REGISTERS
;
;  Copy the registers
        LD      DE,TASK_REGS            ;OUR REGISTERS LIVE HERE
        LD      B,A
WRRLP:  LD      A,(HL)                  ;GET BYTE TO A
        LD      (DE),A                  ;STORE TO REGISTER RAM
        INC     HL
        INC     DE
        DJNZ    WRRLP
;
;  Return OK status
WRR80:  XOR	A
        JP	SEND_STATUS

;===========================================================================
;
;  Run Target:  FN, len
;
;  Uses 4 bytes of stack
;
RUN_TARGET:
;
;  Restore user's page
;;      LD      A,(REG_PAGE)
;;      LD      (PAGEIMAGE),A
;;      LD      BC,PAGELATCH
;;      OUT     (BC),A
;
;  Restore alternate registers
        LD      HL,(REG_AFX)
        PUSH    HL
        POP     AF
        EX      AF,AF'                  ;LOAD ALTERNATE AF
        ;
        LD      HL,(REG_HLX)
        LD      BC,(REG_BCX)
        LD      DE,(REG_DEX)
        EXX                             ;LOAD ALTERNATE REGS
;
;  Restore main registers
        LD      BC,(REG_BC)
        LD      DE,(REG_DE)
        LD      IX,(REG_IX)
        LD      IY,(REG_IY)
        LD      A,(REG_I)
        LD      I,A
;
;  Switch to user stack
        LD      HL,(REG_PC)             ;USER PC
        LD      SP,(REG_SP)             ;BACK TO USER STACK
        PUSH    HL                      ;SAVE USER PC FOR RET
        LD      HL,(REG_AF)
        PUSH    HL                      ;SAVE USER A AND FLAGS FOR POP
        LD      HL,(REG_HL)             ;USER HL
;
;  Restore user's interrupt state
        LD      A,(REG_IFF)
        OR      A
        JR      Z,RUTT10                ;JIF INTS OFF: LEAVE OFF
;
;  Return to user with interrupts enabled
        POP     AF
        EI                              ;ELSE ENABLE THEM NOW
        RET
;
;  Return to user with interrupts disabled
RUTT10: pop	af
        ret

;===========================================================================
;
;  Common continue point for all monitor entrances
;  HL = user PC, SP = user stack
;  REG_STATE has current state, REG_HL, REG_I, REG_IFF, REG_AF set
;
;  Uses 2 bytes of stack
;
ENTER_MON:
        LD      (REG_SP),SP     ;SAVE USER'S STACK POINTER
        LD      SP,MONSTACK     ;AND USE OURS INSTEAD
;
        LD      (REG_PC),HL
        LD      (REG_BC),BC
        LD      (REG_DE),DE
        LD      (REG_IX),IX
        LD      (REG_IY),IY
;
;  Get alternate register set
        EXX
        LD      (REG_HLX),HL
        LD      (REG_BCX),BC
        LD      (REG_DEX),DE
        EX      AF,AF'
        PUSH    AF
        POP     HL
        LD      (REG_AFX),HL
;
;;      LD      A,(PAGEIMAGE)   ;GET CURRENT USER PAGE
        XOR     A               ;...OR NONE IF UNPAGED TARGET
        LD      (REG_PAGE),A    ;SAVE USER PAGE
;
;  Return registers to master
        JP	RETURN_REGS

;===========================================================================
;
;  Set target byte(s):  FN, len { (page, alow, ahigh, data), (...)... }
;
;  Return has FN, len, (data from memory locations)
;
;  If error in insert (memory not writable), abort to return short data
;
;  This function is used primarily to set and clear breakpoints
;
;  Uses 2 bytes of stack
;
SET_BYTES:
;
        LD      HL,COMBUF+1
        LD      B,(HL)                  ;LENGTH = 4*NBYTES
        INC     HL
        INC     B
        DEC     B
        LD      C,0                     ;C GETS COUNT OF INSERTED BYTES
        JR      Z,SB90                  ;JIF NO BYTES (C=0)
        PUSH    HL
        POP     IX                      ;IX POINTS TO RETURN BUFFER
;
;  Loop on inserting bytes
SB10:   LD      A,(HL)                  ;MEMORY PAGE
        INC     HL
;;      LD      (PAGEIMAGE),A
;;      PUSH    BC
;;      LD      BC,PAGELATCH
;;      OUT     (BC),A                  ;SET PAGE
;;      POP     BC
        LD      E,(HL)                  ;ADDRESS TO DE
        INC     HL
        LD      D,(HL)
        INC     HL
;
;  Read current data at byte location
        LD      A,(DE)                  ;READ CURRENT DATA
        LD      (IX),A                  ;SAVE IN RETURN BUFFER
        INC     IX
;
;  Insert new data at byte location
        LD      A,(HL)
        LD      (DE),A                  ;SET BYTE
        LD      A,(DE)                  ;READ IT BACK
        CP      (HL)                    ;COMPARE TO DESIRED VALUE
        JR      NZ,SB90                 ;BR IF INSERT FAILED: ABORT
        INC     HL
        INC     C                       ;ELSE COUNT ONE BYTE TO RETURN
;
        DEC     B
        DEC     B
        DEC     B
        DJNZ    SB10                    ;LOOP FOR ALL BYTES
;
;  Return buffer with data from byte locations
SB90:   LD      A,C
        LD      (COMBUF+1),A            ;SET COUNT OF RETURN BYTES
;
;  Compute checksum on buffer, and send to master, then return
        JP      SEND

;===========================================================================
;
;  Input from port:  FN, len, PortAddressLo, PAhi (=0)
;
IN_PORT:
;
;  Get port address
        LD      BC,(COMBUF+2)
;
;  Read port value
        IN      A,(C)           ;IN WITH A15-A8 = B; A7-A0 = C
;
;  Return byte read as "status"
        JP      SEND_STATUS

;===========================================================================
;
;  Output to port:  FN, len, PortAddressLo, PAhi (=0), data
;
OUT_PORT:
;
;  Get port address
        LD      BC,(COMBUF+2)
;
;  Get data
        LD      A,(COMBUF+4)
;
;  Write value to port
        OUT     (C),A           ;OUT WITH A15-A8 = B; A7-A0 = C
;
;  Return status of OK
        XOR	A
        JP	SEND_STATUS
;
;===========================================================================
;  Build status return with value from "A"
;
SEND_STATUS:
        LD	(COMBUF+2),A		; SET STATUS
        LD	A,1
        LD	(COMBUF+1),A		; SET LENGTH
;;;     JMP     SEND

;===========================================================================
;  Append checksum to COMBUF and send to master
;
;  Uses 6 bytes of stack (not including return address: jumped, not called)
;
SEND:   CALL    CHECKSUM                ;GET A=CHECKSUM, HL->checksum location
        NEG
        LD      (HL),A                  ;STORE NEGATIVE OF CHECKSUM
;
;  Send buffer to master
        LD      HL,COMBUF               ;POINTER TO DATA
        LD      A,(COMBUF+1)            ;LENGTH OF DATA
        ADD     A,3                     ;PLUS FUNCTION, LENGTH, CHECKSUM
        LD      B,A                     ;save count for loop
SND10:  LD      A,(HL)
        CALL    PUTCHAR                 ;SEND A BYTE (uses 6 bytes of stack)
        INC     HL
        DJNZ    SND10
        JP      MAIN                    ;BACK TO MAIN LOOP

;===========================================================================
;  Compute checksum on COMBUF.  COMBUF+1 has length of data,
;  Also include function byte and length byte
;
;  Returns:
;       A = checksum
;       HL = pointer to next byte in buffer (checksum location)
;       B is scratched
;
;  Uses 2 bytes of stack including return address
;
CHECKSUM:
        LD      HL,COMBUF               ;pointer to buffer
        LD      A,(COMBUF+1)            ;length of message
        ADD     A,2                     ;plus function, length
        LD      B,A                     ;save count for loop
        XOR     A                       ;init checksum to 0
CHK10:  ADD     A,(HL)
        INC     HL
        DJNZ    CHK10                   ;loop for all
        RET                             ;return with checksum in A

VRAM_ADR::	DW 0B770h	; VRAM buffer visible start address - 0B7C2h
PPI_ADR::	DW 0C200h 	; VV55 keyboard Controller - 0C200h
PPI2_ADR::	DW 0C400h	; VV55 additional PPI  - 0C400h
DISP_ADR::	DW 0C000h	; VG75 Display Controller - 0C000h
DMA_ADR::	DW 0E000h	; VT57 DMA Controller - 0E000h
PALM_CTR_ADR::	DW 0CE00h	; Palmira Control Byte
PPI3_ADR::	DW 0CA00h	; VV55 additional PPI3  - 0CA00h
		DW 0,0	        ; Reserved for future use

;
;===========================================================================
;  Hardware initialization table
INIOUT:
;
;-----------------------------------------------------------------------
;

;-------------------------------------------------------------------------
OUTCNT  EQU     ($-INIOUT)/2    ; NUMBER OF INITIALIZING PAIRS
        END     RESET
