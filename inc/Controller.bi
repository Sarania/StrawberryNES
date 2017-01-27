Declare Function padRead As Ubyte
Declare sub padWrite
Declare Sub getPad
 
Sub getPad
    button(0) = IIf(MultiKey(SC_RIGHT),1,0)
    button(1) = IIf(MultiKey(SC_LEFT),1,0)
    button(2) = IIf(MultiKey(SC_SPACE),1,0)
    button(3) = IIf(MultiKey(SC_ENTER),1,0)
    button(4) = IIf(MultiKey(SC_W),1,0)
    button(5) = IIf(MultiKey(SC_S),1,0)
    button(6) = IIf(MultiKey(SC_A),1,0)
    button(7) = IIf(MultiKey(SC_D),1,0)
End Sub
 
Sub padWrite
   button_counter = 0
End sub
 
Function padRead As UByte
    Dim As UByte value
    If button_counter > 7 Then
        Return 1
    Else
        getPad
        value = button(button_counter)
        button_counter += 1
    EndIf
    Return value
End Function