; hello world example
; for win2c64 by Aart Bik
; http://www.aartbik.com/

chrout  .equ   $ffd2   ; kernal addresss

main    .org   $c000   ; start at free RAM
        ldx    #0
loop    lda    text,x
        jsr    chrout    
        inx
        cpx    #11
        bne    loop 
        rts
text    .byte  "HELLO WORLD"

