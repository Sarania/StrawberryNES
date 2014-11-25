;
; simple C64 sprite example
; for win2c64 by Aart Bik
; http://www.aartbik.com/
;
sprite0  .equ   $07f8
sprite1  .equ   $07f9
vicbase  .equ   $d000
;
; encode SYS 2064 line
; in BASIC program space
;
        .org  $0800                          
        .byte $00 $0c $08 $0a $00 $9e $20 $32 
        .byte $30 $36 $34 $00 $00 $00 $00 $00
lab2064 jmp  main
;
; bitmap data for sprites at 64-byte aligned block
;
        .align 64
bitmap  .byte %01111111 %11111111 %11111110
        .byte %11000000 %00011000 %00000011
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00111100 %00000001
        .byte %11111111 %11111111 %11111111
        .byte %10000000 %00111100 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %10000000 %00011000 %00000001
        .byte %11000000 %00011000 %00000011
        .byte %01111111 %11111111 %11111110
;
; program data
;
main    ldx #@6 bitmap ; get 64-byte block of bitmap
        stx sprite0    ;
        stx sprite1    ; set sprite pointers
        ldx #100       ;
        stx vicbase+0  ;
        ldx #150       ;
        stx vicbase+1  ;
        stx vicbase+2  ;
        stx vicbase+3  ;
        ldx #0         ; put sprites at 100,150
        stx vicbase+16 ;            and 150,150
        stx vicbase+32 ;
        stx vicbase+33 ; black screen   
        ldx #1         ;
        stx vicbase+39 ; make sprite0 white
        ldx #7         ;
        stx vicbase+40 ; make sprite1 yellow
        ldx #3         ;
        stx vicbase+21 ; turn sprites on
        rts            ; done
