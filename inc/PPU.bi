Declare Sub ppuLoop
Declare function readPPUreg(ByVal addr As ULongInt) As ulongint
Declare Function writePPUreg(ByVal addr As ULongInt, ByVal value As UByte) As ULongInt

Function readPPUreg(ByVal addr As ULongInt)as ulongint
	
End Function

Function writePPUreg(ByVal addr As ULongInt, ByVal value As UByte) As ULongInt
	
End Function
Sub ppuLoop
	If totalops > 27000 Then ppu.scanline +=1
	If ppu.scanline > 240 Then
		nmi
		ppu.scanline = 0
	EndIf
End Sub
