Declare Sub ppuLoop
Declare function readPPUreg(ByVal addr As UShort) As ulongint
Declare Function writePPUreg(ByVal addr As uShort, ByVal value As UByte) As ULongInt

Function readPPUreg(ByVal addr As UShort)as ulongint
readPPUreg = cpu.memory(addr)
End Function

Function writePPUreg(ByVal addr As UShort, ByVal value As UByte) As ULongInt
	Select Case(addr)
	Case &h2000
	PPUCTRL = value 
	Case &h2001
	PPUMASK = value
	Case &h2003
	ppu.sprAddr = value Or ((ppu.sprAddr And &hFF) Shl 8)
	Case &h2004
	ppu.sprRAM(ppu.sprAddr+1) = value
	ppu.sprAddr Or= &hFF
	Case &h2005
	PPUSCROLL = value Or ((PPUSCROLL And &hFF) Shl 8)
	Case &h2006
	ppu.vrAddr = value Or ((ppu.vrAddr And &hFF)Shl 8)
	Case &h2007
	Dim As UShort vLocation = ppu.vrAddr
	Dim As UShort vTempLoc = (ppu.vrAddr And &h3f10)
	If(vTempLoc < &h10) Then vLocation -= &h10
	ppu.vram(vLocation) = value
	If PPUCTRL_I = 1 Then 
		ppu.vrAddr += 32
	Else ppu.vrAddr += 1
	EndIf
	End Select
End Function
Sub ppuLoop
	If totalops > 27000 Then ppu.scanline +=1
	If ppu.scanline > 240 Then
		If PPUCTRL_V = 1 Then nmi
		ppu.scanline = 0
	EndIf

End Sub
