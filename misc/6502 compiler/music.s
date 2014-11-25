;
; simple C64 music example
; for win2c64 by Aart Bik
; http://www.aartbik.com/
;
sidbase  .equ   $d400
;
; encode SYS 2064 line
; in BASIC program space
;
        .org  $0800                          
        .byte $00 $0c $08 $0a $00 $9e $20 $32 
        .byte $30 $36 $34 $00 $00 $00 $00 $00
lab2064 jmp  main
;
; melody data
;
melody   .byte    8,  97, 10, 143, 12, 143
         .byte    8,  97, 11,  48, 14,  24
         .byte    9, 104, 12, 143, 15, 210
         .byte   10, 143, 12, 143, 16, 195
;
; program data
;
main    ldx #9          ;
        stx sidbase+5   ;
        stx sidbase+12  ; attack/decay
        stx sidbase+19  ; voice1,2,3
        ldx #0          ;
        stx sidbase+6   ;
        stx sidbase+13  ; sustain/release
        stx sidbase+20  ; voice1,2,3
        ldx #15         ;
        stx sidbase+24  ; volume on
start   ldy #0          ;
play    ldx #0          ;
        stx sidbase+4   ;
        stx sidbase+11  ; reset waves
        stx sidbase+18  ; voice1,2,3
        lda melody, y   ;
        sta sidbase+1   ;
        iny             ;
        lda melody, y   ;
        sta sidbase+0   ; freq voice1
        iny             ;
        lda melody, y   ;
        sta sidbase+8   ;
        iny             ;
        lda melody, y   ;
        sta sidbase+7   ; freq voice2
        iny             ;
        lda melody, y   ;
        sta sidbase+15  ;
        iny             ;
        lda melody, y   ;
        sta sidbase+14  ; freq voice3
        iny             ;
        ldx #33         ;
        stx sidbase+4   ;
        stx sidbase+11  ; set waves
        stx sidbase+18  ; voice1,2,3
        tya             ;
        ldy #0          ;
loop1   ldx #0          ;
loop2   inx             ;
        bne loop2       ;
        iny             ;
        bne loop1       ; delay
        tay             ;
        cpy #24         ;
        bne play        ;
        rts             ; done
