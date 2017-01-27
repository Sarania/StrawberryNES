Declare Sub ppuLoop
Declare Sub ProcessCurTile
Declare Function readPPUreg(ByVal addr As UShort)as ULongInt
Declare Function writePPUreg(ByVal addr As uShort, ByVal value As UByte) As ULongInt

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
				ppu.vrAddr And= &h3FFF
			endif
		Case &h2007
			ppu.vram(ppu.vrAddr) = value
			If PPUCTRL_I = 1 Then
				ppu.vrAddr += 32
			Else ppu.vrAddr += 1
			EndIf
			ppu.vrAddr And= &h3FFF
	End Select
End Function
Function readPPUreg(ByVal addr As UShort)As ULongInt
	Dim As UByte value
	Select Case addr
		Case &h2002
			value = PPUSTATUS
			PPUSTATUS = PPUSTATUS And &h7F
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

Sub ProcessCurTile
Dim As Ubyte pixel
Dim As UInteger palette_address = &h3f01 + (ppu.palette * 4)
Dim as uinteger pPalette = ppu.vram(&h3f00) + (ppu.vram(palette_address) Shl 8) + (ppu.vram(palette_address+1) Shl 16) + (ppu.vram(palette_address+2) Shl 24)
For zz As Integer = 0 To 7
pixel =((ppu.lbit Shr 7) and &h1) + (((ppu.ubit Shr 7) and &h1) Shl 1)
'PSet framebuffer, (ppu.curx,ppu.cury), masterpalette((pPalette Shr (pixel * 8) AND &hff))
'PSet framebuffer, (ppu.curx,ppu.cury), masterpalette((pPalette Shr (pixel * 8) AND &hff))
Line framebuffer, ((ppu.curx*2)-2,(ppu.cury*2)-1)-((ppu.curx*2),(ppu.cury*2)-1), masterpalette((pPalette Shr (pixel * 8) AND &hff))
Line framebuffer, ((ppu.curx*2)-2,(ppu.cury*2)-2)-((ppu.curx*2),(ppu.cury*2)-2), masterpalette((pPalette Shr (pixel * 8) AND &hff))
ppu.curx+=1
ppu.lbit Shl = 1
ppu.ubit Shl = 1
If ppu.curx = 256 Then
ppu.cury+=1
ppu.curx=0
EndIf
Next
End Sub

Sub ppuLoop
	Select Case ppu.scanline
		Case -1 'prerender scanline
			PPUSTATUS And= &h7F
			framebuffer=ImageCreate(512,480,RGB(0,0,0))
		Case 0 To 239 'proper scanline
			Dim temp_P As UInteger
			ppu.tableLine = PPUCTRL_NN + ((ppu.scanline \ 8) * 32)
			ppu.attrbLine = PPUCTRL_NN + &h3C0 + ((ppu.scanline \ 32) * 8)
			For z As UByte = 0 To 31
				ppu.curAttrb = ppu.vram(ppu.attrbLine + (z \ 4))
				ppu.curtile = ppu.vram(ppu.tableLine+z) 'this contains the pattern lookup from the nametable
				If Not (ppu.scanline\16) And 1 Then
					If (z\2) And 1 Then
						ppu.palette = (ppu.curAttrb Shr 2) And &h3
					Else
						ppu.palette = ppu.curAttrb And &h3
					EndIf
				Else
					If(z\2) And 1 Then
						ppu.palette=(ppu.curAttrb Shr 6) And &h3
					Else
						ppu.palette=(ppu.curAttrb Shr 4) And &h3
					EndIf
				End If
				temp_p = PPUCTRL_B + (ppu.scanline AND 7)
				ppu.lbit = ppu.vram(((temp_P + (ppu.curTile * 16))))
				ppu.ubit = ppu.vram(((temp_p + (ppu.CurTile * 16)  + 8)))
				ProcessCurTile
			Next
		Case 240 'post render scanline
			Put(screenx-512,screeny-480),framebuffer,PSet
			ImageDestroy(framebuffer)
		Case Else 'vblank period
			'stuff
			ppu.curx=0
			ppu.cury=0
	End Select
	If ppu.scanline = 241 Then
		PPUSTATUS Or= &h80
		If PPUCTRL_V = 1 Then
			nmi
		EndIf
	EndIf
	ppu.scanline += 1
	If ppu.scanline = 262 Then ppu.scanline = -1
End Sub
