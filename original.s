;
;ProDOS.disk.driver
;for
;64k/128k.Saturn.ramcard
;
;written.by.Matthew.Lee.Stier
;Copyright.(c).1986
;
;
JMP       EQU  $4C
LDA       EQU  $B1            ;INDIRECT,Y
STA       EQU  $91            ;INDIRECT,Y
FORMAT.C  EQU  $4
WR.BLK.C  EQU  $81
GET.TIME  EQU  $82
NO.EC     EQU  $00
IO.EC     EQU  $27
WP.EC     EQU  $28
ZPAGE     EQU  $0
CMD       EQU  $42
BUFFER    EQU  $44
BLK.NUM   EQU  $46
MLI       EQU  $BF00
DATETIME  EQU  $BF06
DEVADR31  EQU  $BF16
DEVCNT    EQU  $BF31
DEVLST    EQU  $BF32
COPYRITE  EQU  $BF40
DATE      EQU  $BF90
LC        EQU  $C080
ROMBNK2WE EQU  LC+$1
LCBNK2WE  EQU  LC+$3
LCBNK1WE  EQU  LC+$B
BLK0      EQU  LC+$4
ROMBNK1WE EQU  LC+$9
BLK7      EQU  LC+$F
;
          ORG  $2000
          LST  ON,NOA,NOV,GEN
;
          LDY  #7
NXTBLK16  LDX  LC.INDEX,Y     ;TEST FOR RAMCARD
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
LC.INDEX  DB   $4,$5,$6,$7,$C,$D,$E,$F
BLKSIZE   DW   0,0,0,96,0,0,0,224
;
INSTALL   STX  MAX.BLK+1      ;SET I/O ROUTINE
          STA  MAX.BLK        ; MAX BLOCK
          STX  B2.D+1+$2A     ;SET DIRECTORY 
          STA  B2.D+1+$29     ; MAX BLOCK
          CPY  #3*2           ;IF 64K RAMCARD
          BNE  VBM128
          LDY  B1.96          ;USE 96 BLOCK VBM 
SET.VBM   LDA  B1.96,Y
          STA  B1.D,Y
          DEY
          BPL  SET.VBM
VBM128    JSR  MLI            ;GET CURRENT TIME
          DB   GET.TIME
          DW   0000
          LDY  #3
MOV.DATE  LDA  DATE,Y         ;MOVE IT TO DIRECTORY
          STA  B2.D+1+$1C,Y   ; HEADER
          DEY
          BPL  MOV.DATE
DO.MOVE1  BIT  ROMBNK2WE
          BIT  ROMBNK2WE
          LDY  #0
MOVE1     LDA  CHCK.CMD,Y     ;MOVE I/O ROUTINE
          STA  CHCK.CMD+OS,Y  ; TO BANK 2 @ $D000
          INY
          BNE  MOVE1
          LDY  #15
MOVE2     LDA  LC.ENTER,Y     ;MOVE LC.ENTER
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
          LDX  #<B2.D
          LDY  #>B2.D
          JSR  WR.BLOCK
          LDA  #3             ;LAST DIRCETORY BLOCK
          LDX  #<B3.D
          LDY  #>B3.D
          JSR  WR.BLOCK
          LDA  #1             ;VOLUME BITMAP
          LDX  #<B1.D
          LDY  #>B1.D
          JSR  WR.BLOCK
          CLC
          RTS                 ;AND END
;
WR.BLOCK  STA  WR.BLK.P+4     ;SETUP BLK.NUM
          LDA  #00
          STA  WR.BLK.P+5
          STX  LENGTH+2       ;SETUP DATA POINTER
          STY  LENGTH+1
          STX  MOVDATA+2
          STY  MOVDATA+1
          LDY  #0 
          LDA  #0
ZBUFR     STA  BW.BUFR,Y      ;ZERO I/O BUFFER
          STA  BW.BUFR+256,Y
          INY
          BNE  ZBUFR
LENGTH    LDX  BW.BUFR        ;GET LENGTH OF INFO
MOVDATA   LDA  BW.BUFR,X
          STA  BW.BUFR-1,X    ;MOVE DATA TO I/O BUFFER
          DEX
          BNE  MOVDATA
          JSR  MLI            ;AND WRITE IT TO /RAM
          DB   WR.BLK.C
          DW   WR.BLK.P
          RTS
;
WR.BLK.P  DB   3              ;NUMBER OF PARAMETERS
          DB   $30            ;SLOT/DRIVE NUMBER
          DW   BW.BUFR        ;DATA BUFFER
          DW   0000           ;BLOCK NUMBER
;
B2.D      DB   B2.END-*-1
          DW   0              ;PREVIOUS.DIRECTORY.BLOCK
          DW   3              ;NEXT.DIRECTORY.BLOCK
VTYPE     DB   $F3            ;VTYPE/VN.LEN
VNAME     ASC  'RAM'          ;VNAME
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
B2.END    EQU  *
;
B3.D      DB   B3.END-*-1
          DW   2              ;PREVIOUS.DIRECTORY.BLOCK
          DW   0              ;NEXT.DIRECTORY.BLOCK
B3.END    EQU  *
;
B1.D      DB   B1.END-*-1
          DB   %00001111      ;BLOCKS 0,1,2,3 ARE USED
          DS   27,$FF         ;ALL THE REST ARE FREE
B1.END    EQU  *
;
B1.96     DB   B1.96END-*-1
          DB   $0F            ;BLOCKS 0,1,2,3 ARE USED
          DS   11,$FF         ;ALL THE REST ARE FREE
B1.96END  EQU  *
;
LC.ENTER  CLD                 ;ALL ROUTINE BEGIN WITH
          BIT  LCBNK2WE       ;ENABLE BANK 2
          JSR  CHCK.CMD+OS    ; AND GOTO IT
LC.EXIT   PHP                 ;SAVE THE STATUS REGISTER
          BIT  LCBNK1WE       ;ENABLE BANK 1
          BIT  LCBNK1WE
          PLP                 ;RESTORE THE STATUS REG.
          RTS                 ;AND RETURN
;
          DS   >0-*,0
OS        EQU  $D000-*        ;OFFSET TO LANGUAGE CARD
;
CHCK.CMD  BIT  LCBNK2WE       ;ENSURE BANK 2 IS SELECTED
          LDA  CMD            ;GET THE COMMAND
          BEQ  STATUS         ; IS IT 'STATUS'...
          CMP  #FORMAT.C      ; IS IT 'FORMAT'
          BCC  RD.WR          ; NO, READ OR WRITE...
          BNE  IO.EXIT        , NO, I/O ERROR
          LDA  #WP.EC         ;ITS FORMAT
          SEC                 ; MARK DRIVE AS
          RTS                 ; WRITE PROTECTED
;
IO.EXIT   LDA  #IO.EC         ;ITS AN I/O ERROR
          SEC
          RTS
;
STATUS    LDY  MAX.BLK+OS+1   ;GET VOLUME SIZE
          LDX  MAX.BLK+OS
STATUS2   LDA  #NO.EC         ;AND RETURN
          CLC
          RTS
;
MAX.BLK   DW   0              ;NUMBER OF BLOCKS
;
RD.WR     LDA  BLK.NUM+1      ;CHECK VALID BLOCK NUM
          CMP  MAX.BLK+OS+1
          BNE  RD.WR2
          LDA  BLK.NUM
          CMP  MAX.BLK+OS     ;IF GREATER THAN
RD.WR2    BCS  IO.EXIT        ; MARK AS I/O ERROR...
          PHA
          LSR  A              ;GET RAMCARD
          LSR  A              ; BLOCK NUMBER
          LSR  A
          LSR  A
          LSR  A
          TAY
          LDA  B16TBL+OS,Y    ;CONVERT TO I/O ADDRESS
          STA  RC.BLK+OS+1
          PLA
          AND  #%00011111     ;GET BLOCK NUMBER
          CMP  #8
          BCS  NO.FIX         ;SET CORRECT BANK
          ADC  #8
NO.FIX    PHA
          LDA  #LCBNK2WE
          BCC  NO.FIX2
          LDA  #LCBNK1WE
NO.FIX2   STA  RC.BNK+OS+1
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
RC.BLK    LDA  #00
          STA  MOVLOOP+OS+1,Y
RC.BNK    LDA  #00
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
          LDA  #LCBNK1WE
          STA  MOVLOOP+OS+1,Y
          LDA  #BLK0
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
SWAP      LDY  #RWR.END-RWR   ;GET LENGTH OF RWR
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
          DB   LDA,S1-RWR     ;GET THE DATA
          STA  T1-RWR
          DB   LDA,S2-RWR
          STA  T2-RWR
          BIT  LC             ;ENABLE DESTINATION
          BIT  LC
          LDA  T1-RWR         ;SAVE THE DATA
          DB   STA,D1-RWR
          LDA  T2-RWR
          DB   STA,D2-RWR
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
RWR.END   EQU  *
;
          DS   >0-*,0
BW.BUFR   EQU  *              ;I/O BUFFER
*
