; Converted to Merlin32 by Michaelangel007
;   https://github.com/Michaelangel007/apple2_saturn_ramdrive
; Originally from:
;   http://mirrors.apple2.org.za/apple.cabi.net/FAQs.and.INFO/LanguagesAndProgramming/saturn.ram.src.txt
; Comment out for cc65
;.include "fix_cc65.s"

;
;ProDOS.disk.driver
;for
;64k/128k.Saturn.ramcard
;
;written.by.Matthew.Lee.Stier
;Copyright.(c).1986
;
;
 
; 6502 Opcodes
;JMP       EQU  $4C
;LDA       EQU  $B1            ;INDIRECT,Y
;STA       EQU  $91            ;INDIRECT,Y

;4C JMP (abs)
;B1 LDA (ZP),Y
;91 STA (ZP),Y

FORMAT_C  EQU  $4

; ProDOS MLI
WR_BLK_C  EQU  $81
GET_TIME  EQU  $82

; Error Codes
NO_EC     EQU  $00
IO_EC     EQU  $27
WP_EC     EQU  $28

ZPAGE     EQU  $0
CMD       EQU  $42
BUFFER    EQU  $44
BLK_NUM   EQU  $46

; ProDOS
MLI       EQU  $BF00
DATETIME  EQU  $BF06
DEVADR31  EQU  $BF16
DEVCNT    EQU  $BF31
DEVLST    EQU  $BF32
COPYRITE  EQU  $BF40
DATE      EQU  $BF90

; IO Softswitches
LC        EQU  $C080
ROMBNK2WE EQU  LC+$1
LCBNK2WE  EQU  LC+$3
LCBNK1WE  EQU  LC+$B
BLK0      EQU  LC+$4
ROMBNK1WE EQU  LC+$9
BLK7      EQU  LC+$F
;
          ORG $2000
          LST  ON,NOA,NOV,GEN
;
          LDY  #7
NXTBLK16  LDX  LC_INDEX,Y     ;TEST FOR RAMCARD
          LDA  LC,X           ; SIZE
          BIT  LCBNK2WE       ;ENABLE BANK 2
          BIT  LCBNK2WE
          TYA
          STA  $D000
          DEY
          BPL  NXTBLK16
          BIT  BLK7
          LDY  $D000          ;GET RAMCARD SIZE
          BIT  BLK0           ;RESTORE BLOCK 0
          BIT  ROMBNK1WE      ; BANK 1
          BIT  ROMBNK1WE
          CPY  #8             ;ILLEGAL BANK SIZE?
          BCS  TESTRTN        ; YES...
          TYA
          ASL
          TAY
          LDX  BLKSIZE+1,Y    ;GET RAMCARD SIZE
          LDA  BLKSIZE,Y      ; IN PRODOS BLOCKS
          BNE  INSTALL        ;IF NOT 0 THEN INSTALL
          SEC                 ;ELSE FLAG AN ERROR
TESTRTN   RTS                 ; AND QUIT
;
LC_INDEX  DB   $4,$5,$6,$7,$C,$D,$E,$F
BLKSIZE   DW   0,0,0,96,0,0,0,224

INSTALL   STX  MAX_BLK+1      ;SET I/O ROUTINE
          STA  MAX_BLK        ; MAX BLOCK
          STX  B2_D+1+$2A     ;SET DIRECTORY 
          STA  B2_D+1+$29     ; MAX BLOCK
          CPY  #3*2           ;IF 64K RAMCARD
          BNE  VBM128
          LDY  B1_96          ;USE 96 BLOCK VBM 
SET_VBM   LDA  B1_96,Y
          STA  B1_D,Y
          DEY
          BPL  SET_VBM
VBM128    JSR  MLI            ;GET CURRENT TIME
          DB   GET_TIME
          DW   0000
          LDY  #3
MOV_DATE  LDA  DATE,Y         ;MOVE IT TO DIRECTORY
          STA  B2_D+1+$1C,Y   ; HEADER
          DEY
          BPL  MOV_DATE
DO_MOVE1  BIT  ROMBNK2WE
          BIT  ROMBNK2WE
          LDY  #0
MOVE1     LDA  CHCK_CMD,Y     ;MOVE I/O ROUTINE
          STA  CHCK_CMD+OS,Y  ; TO BANK 2 @ $D000
          INY
          BNE  MOVE1
          LDY  #15
MOVE2     LDA  LC_ENTER,Y     ;MOVE LC_ENTER
          STA  COPYRITE,Y     ; TO PRODOS GLOBAL
          DEY                 ; PAGE
          BPL  MOVE2
          LDA  #<COPYRITE     ;INSTALL RAMCARD
          STA  DEVADR31+1     ; VECTOR IN DRIVE TABLE
          LDA  #>COPYRITE
          STA  DEVADR31
          INC  DEVCNT         ;INSTALL RAMCARD IN
          LDY  DEVCNT         ; LIST OF ATTACHED DRIVES
          LDA  #$3F           ;SLOT 3, DRIVE 2, TYPE-RAM
          STA  DEVLST,Y
          LDA  #2             ;FIRST DIRECTORY BLOCK
          LDX  #<B2_D
          LDY  #>B2_D
          JSR  WR_BLOCK
          LDA  #3             ;LAST DIRCETORY BLOCK
          LDX  #<B3_D
          LDY  #>B3_D
          JSR  WR_BLOCK
          LDA  #1             ;VOLUME BITMAP
          LDX  #<B1_D
          LDY  #>B1_D
          JSR  WR_BLOCK
          CLC
          RTS                 ;AND END
;
WR_BLOCK  STA  WR_BLK_P+4     ;SETUP BLK_NUM
          LDA  #00
          STA  WR_BLK_P+5
          STX  LENGTH+2       ;SETUP DATA POINTER
          STY  LENGTH+1
          STX  MOVDATA+2
          STY  MOVDATA+1
          LDY  #0 
          LDA  #0
ZBUFR     STA  BW_BUFR,Y      ;ZERO I/O BUFFER
          STA  BW_BUFR+256,Y
          INY
          BNE  ZBUFR
LENGTH    LDX  BW_BUFR        ;GET LENGTH OF INFO
MOVDATA   LDA  BW_BUFR,X
          STA  BW_BUFR-1,X    ;MOVE DATA TO I/O BUFFER
          DEX
          BNE  MOVDATA
          JSR  MLI            ;AND WRITE IT TO /RAM
          DB   WR_BLK_C
          DW   WR_BLK_P
          RTS
;
WR_BLK_P  DB   3              ;NUMBER OF PARAMETERS
          DB   $30            ;SLOT/DRIVE NUMBER
          DW   BW_BUFR        ;DATA BUFFER
          DW   0000           ;BLOCK NUMBER
;
B2_D      DB   B2_END-*-1
          DW   0              ;PREVIOUS.DIRECTORY.BLOCK
          DW   3              ;NEXT.DIRECTORY.BLOCK
VTYPE     DB   $F3            ;VTYPE/VN.LEN
VNAME     ASC  "RAM"          ;VNAME ; bugfix: VNAME     ASC  'RAM'          ;VNAME
          DS   VNAME+15-*,0
          DS   8,0            ;RESERVED
          DW   0              ;CREATION.DATE
          DW   0              ;CREATION.TIME
          DB   1              ;CREATION.VERSION
          DB   0              ;MINIMUM.VERSION
          DB   %01000011      ;ACCESS BITS
          DB   39             ;SIZE DIR ENTRIES
          DB   13             ;ENTRIES PER BLOCK
          DW   0              ;ACTIVE FILES
          DW   1              ;VBM
          DW   0              ;# OF BLOCKS
B2_END    EQU  *
;
B3_D      DB   B3_END-*-1
          DW   2              ;PREVIOUS.DIRECTORY.BLOCK
          DW   0              ;NEXT.DIRECTORY.BLOCK
B3_END    EQU  *
;
B1_D      DB   B1_END-*-1
          DB   %00001111      ;BLOCKS 0,1,2,3 ARE USED
          DS   27,$FF         ;ALL THE REST ARE FREE
B1_END    EQU  *
;
B1_96     DB   B1_96END-*-1
          DB   $0F            ;BLOCKS 0,1,2,3 ARE USED
          DS   11,$FF         ;ALL THE REST ARE FREE
B1_96END  EQU  *
;
LC_ENTER  CLD                 ;ALL ROUTINE BEGIN WITH
          BIT  LCBNK2WE       ;ENABLE BANK 2
          JSR  CHCK_CMD+OS    ; AND GOTO IT
LC_EXIT   PHP                 ;SAVE THE STATUS REGISTER
          BIT  LCBNK1WE       ;ENABLE BANK 1
          BIT  LCBNK1WE
          PLP                 ;RESTORE THE STATUS REG.
          RTS                 ;AND RETURN
;
          DS   >*-*,0 ; **sigh** cc65 range error: bugfix: >0-* to >*-*
OS        EQU  $D000-*        ;OFFSET TO LANGUAGE CARD
;
CHCK_CMD  BIT  LCBNK2WE       ;ENSURE BANK 2 IS SELECTED
          LDA  CMD            ;GET THE COMMAND
          BEQ  STATUS         ; IS IT 'STATUS'...
          CMP  #FORMAT_C      ; IS IT 'FORMAT'
          BCC  RD_WR          ; NO, READ OR WRITE...
          BNE  IO_EXIT        ; NO, I/O ERROR ; bugfix
          LDA  #WP_EC         ;ITS FORMAT
          SEC                 ; MARK DRIVE AS
          RTS                 ; WRITE PROTECTED
;
IO_EXIT   LDA  #IO_EC         ;ITS AN I/O ERROR
          SEC
          RTS
;
STATUS    LDY  MAX_BLK+OS+1   ;GET VOLUME SIZE
          LDX  MAX_BLK+OS
STATUS2   LDA  #NO_EC         ;AND RETURN
          CLC
          RTS
;
MAX_BLK   DW   0              ;NUMBER OF BLOCKS
;
RD_WR     LDA  BLK_NUM+1      ;CHECK VALID BLOCK NUM
          CMP  MAX_BLK+OS+1
          BNE  RD_WR2
          LDA  BLK_NUM
          CMP  MAX_BLK+OS     ;IF GREATER THAN
RD_WR2    BCS  IO_EXIT        ; MARK AS I/O ERROR...
          PHA
          LSR  A              ;GET RAMCARD
          LSR  A              ; BLOCK NUMBER
          LSR  A
          LSR  A
          LSR  A
          TAY
          LDA  B16TBL+OS,Y    ;CONVERT TO I/O ADDRESS
          STA  RC_BLK+OS+1
          PLA
          AND  #%00011111     ;GET BLOCK NUMBER
          CMP  #8
          BCS  NO_FIX         ;SET CORRECT BANK
          ADC  #8
NO_FIX    PHA
          LDA  LCBNK2WE ; bugfix: #
          BCC  NO_FIX2
          LDA  LCBNK1WE ; bugfix: #
NO_FIX2   STA  RC_BNK+OS+1
          PLA
          ASL  A
          ADC  #$C0           ;AND RAMCARD ADDRESS
          TAX
          LDA  CMD
          AND  #%00000010     ;SETUP RAMCARD POINTERS
          TAY
          LDA  #00
          STA  S1+OS,Y
          STA  S2+OS,Y
          TXA
          STA  S1+OS+1,Y
          INX
          TXA
          STA  S2+OS+1,Y
          TYA
          ASL
          ASL
          ASL
          TAY
RC_BLK    LDA  #00
          STA  MOVLOOP+OS+1,Y
RC_BNK    LDA  #00
          STA  MOVLOOP+OS+4,Y
          LDA  CMD
          EOR  #%00000011     ;SETUP MAIN MEM. POINTERS
          AND  #%00000010
          TAY
          LDX  BUFFER+1
          LDA  BUFFER
          STA  S1+OS,Y
          STA  S2+OS,Y
          TXA
          STA  S1+OS+1,Y
          INX
          TXA
          STA  S2+OS+1,Y
          TYA
          ASL
          ASL
          ASL
          TAY
          LDA  LCBNK1WE ; bugfix
          STA  MOVLOOP+OS+1,Y
          LDA  BLK0 ; bugfix
          STA  MOVLOOP+OS+4,Y
          PHP
          SEI                 ;DISABLE INTERUPTS
          JSR  SWAP+OS        ;PUT R/W IN ZPAGE
          JSR  ZPAGE          ; DO READ/WRITE
          JSR  SWAP+OS        ;PUT ZPAGE BACK
          PLP                 ;RESTORE INTERUPT FLAG
          JMP  STATUS2+OS     ;AND EXIT
;
B16TBL    DB   $85,$86,$87,$8C,$8D,$8E,$8F
;
SWAP      LDY  #RWR_END-RWR   ;GET LENGTH OF RWR
SWAP2     LDX  RWR+OS,Y       ;AND SWAP ZPAGE
          LDA  ZPAGE,Y        ; WITH RWR
          STA  RWR+OS,Y
          STX  ZPAGE,Y
          DEY
          BPL  SWAP2
          RTS
;
RWR       LDY  #00            ;INIT INDEX POINTER
MOVLOOP   BIT  LC             ;ENABLE SOURCE
          BIT  LC
          LDA (S1-RWR),Y      ; bugfix: DB   LDA,S1-RWR     ;GET THE DATA
          STA  T1-RWR
          LDA (S2-RWR),Y      ; bugfix: DB   LDA,S2-RWR
          STA  T2-RWR
          BIT  LC             ;ENABLE DESTINATION
          BIT  LC
          LDA  T1-RWR         ;SAVE THE DATA
          STA  (D1-RWR),Y     ; bugfix: DB   STA,D1-RWR
          LDA  T2-RWR
          STA (D2-RWR),Y      ; bugfix: DB   STA,D2-RWR
          INY                 ;DONE?
          BNE  MOVLOOP        ;NO...
          BIT  BLK0           ;RESET TO RAMCARD
          BIT  LCBNK2WE       ; BLOCK 0, BANK 2
          RTS                 ; AND RETURN
;
S1        DW   0              ;SOURCE 1
D1        DW   0              ;DESTINATION 1
S2        DW   0              ;SOURCE 2
D2        DW   0              ;DESTINATION 2
T1        DB   0              ;DATA 1
T2        DB   0              ;DATA 2
RWR_END   EQU  *
;
          DS   >0-*,0
BW_BUFR   EQU  *              ;I/O BUFFER

