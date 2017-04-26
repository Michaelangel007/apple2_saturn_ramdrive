; Version 4
; Utility macros because ca65 is crap out-of-the-box for Apple 2 assembly
; It is missing half of the essential pseudo directives.
; This fixes most of that oversight.

.feature c_comments
.feature labels_without_colons
.feature leading_dot_in_identifiers

;.PC02 ; Uncomment if 65C02 is needed/wanted


; Force APPLE 'text' to have high bit on
; Will display as NORMAL characters
.macro ASC text
    .repeat .strlen(text), I
        .byte   .strat(text, I) | $80
    .endrep
.endmacro


; Force APPLE 'text' with high bit on but last character has high bit off
; Will display as NORMAL characters (last character will appear FLASHING)
; Merlin: Macro Assembler -- Dextral Character Inverted
.macro DCI text
    .repeat .strlen(text)-1, I
        .byte   .strat(text, I) | $80
    .endrep
    .byte   .strat(text, .strlen(text)-1) & $7F
.endmacro


.macro DB b1,b2,b3,b4,b5,b6,b7,b8
    .ifnblank b1
        .byte b1
    .endif
    .ifnblank b2
        .byte b2
    .endif
    .ifnblank b3
        .byte b3
    .endif
    .ifnblank b4
        .byte b4
    .endif
    .ifnblank b5
        .byte b5
    .endif
    .ifnblank b6
        .byte b6
    .endif
    .ifnblank b7
        .byte b7
    .endif
    .ifnblank b8
        .byte b8
    .endif
.endmacro


.macro DS size,fill
    ; Work around cc65 bug
    ;   .res address-*
    ; doesn't work due to bogus "Error: Constant expression expected"
    .local _pc_
_pc_:

    .if .const(_pc_)
        ;.out .sprintf( "CONST * = %04X, reserving $%04X Bytes, New * = $%04X", _pc_, (size-_pc_), size )

        .if (size < 0)
;           .out .sprintf( "INFO: .ds size = %04X < 0, treating as %04X", size, _pc_+size )
            .ifblank fill
                 .res size+_pc_
            .else
                 .res size+_pc_,fill
            .endif
        .else
;           .out .sprintf( "... reserving: %04X bytes ...", size )
;            .res _pc_ - size, fill
            .ifblank fill
                .res size
            .else
                .res size,fill
            .endif
        .endif
    .endif
.endmacro


.macro DW w1,w2,w3,w4,w5,w6,w7,w8
    .ifnblank w1
        .word w1
    .endif
    .ifnblank w2
        .word w2
    .endif
    .ifnblank w3
        .word w3
    .endif
    .ifnblank w4
        .word w4
    .endif
    .ifnblank w5
        .word w5
    .endif
    .ifnblank w6
        .word w6
    .endif
    .ifnblank w7
        .word w7
    .endif
    .ifnblank w8
        .word w8
    .endif
.endmacro


.define EQU =


.macro LST param1,param2,param3,param4
.endmacro


.macro ORG addr
    .org addr
.endmacro

