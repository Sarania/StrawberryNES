	Dim As UByte i
Dim As UByte x
Declare Sub fetchScanline
Declare Sub drawatile
Declare Sub ppuLoop
Declare Function readPPUreg(ByVal addr As UShort)as ULongInt
Declare Function writePPUreg(ByVal addr As uShort, ByVal value As UByte) As ULongInt

Declare Sub FetchBackgroundTile(ByVal Ypos As UInteger, ByVal Xpos As uinteger)

Sub fetchScanline
	Dim As Byte Tile
	Dim As UShort tilesDrawn
	Dim As UShort BaseNameTableAddress=&h2000 + (PPUCTRL_NN * &h400)
	For i As Integer = 0 To 7
	Tile = ppu.vram(BaseNameTableAddress + tilesDrawn)
	frameBuffer((tilesDrawn*8)+i,i) = (ppu.vram(Tile*16)Shr (7-i) And 1)
	frameBuffer((tilesDrawn*8)+i,i) = ((ppu.vram((Tile*16)+8) Shr (7-i) And 1)Shl 1)
	Next
End Sub
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
	ppu.sprAddr +=1
	Case &h2005
	PPUSCROLL = value Or ((PPUSCROLL And &hFF) Shl 8)
		Case &h2006 
	If ppu.addrLatch = 0 Then 
		ppu.vrAddr = 0 
		ppu.vrAddr = (value Shl 8)
		ppu.addrLatch = 1
	Else
		ppu.addrLatch = 0
		ppu.vrAddr Or = value
		ppu.vrAddr And= &h4000
	endif
		Case &h2007	
	ppu.vram(ppu.vrAddr) = value
	If PPUCTRL_I = 1 Then 
		ppu.vrAddr += 32
	Else ppu.vrAddr += 1
	EndIf
	End Select
End Function
Function readPPUreg(ByVal addr As UShort)as ULongInt
	Dim As UByte value
Select Case addr
    Case &h2002
        value = PPUSTATUS
        PPUSTATUS = PPUSTATUS And 127
       ' ppu.addrLatch = 0 
        'clear latch and scroll latch
    Case &h2004
        value = ppu.sprRAM(ppu.sprADDR)
    Case &h2007
        value = ppu.vram(ppu.vrAddr)
    Case else
        'do
End Select
Return value
End Function
Sub drawtile
	For pix As UByte = 0 To 7
	cpu.memory(&h200+pix) = ((rom(&h8010)Shr (7-pix)) And 1)
Next
End Sub
Sub ppuLoop
	
'	fetchScanline
For i As Integer = 0 To 7
	frameBuffer(i,ppu.scanline) = clr(rom(&h8010+ppu.scanline) Shr (7-i)And 1) 'Or (clr(rom(&h8018+ppu.scanline) Shr (7-i)And 1)Shl 1)
	'frameBuffer(i,ppu.scanline) or= (clr(rom(&h8018+ppu.scanline) Shr (7-i)And 1)Shl 1)
Next
	If ppu.scanline > 240 Then
		If PPUCTRL_V = 1 Then 
		PPUSTATUS Or= &h80
		nmi
		EndIf
		ppu.scanline = 0
	EndIf
	ppu.scanline += 1
End Sub
