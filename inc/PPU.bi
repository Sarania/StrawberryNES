Declare Sub ppuLoop
Declare Sub ProcessCurTile
Declare Sub copySprites
Declare Sub renderSprites
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
		Case &h4014
			Dim As UShort dmaaddr = value * &H100
			for i as Uinteger = 0 to &hFF
				ppu.sprRAM(i) = cpu.memory(dmaaddr+i)
			Next
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
	Dim As UInteger xoff, yoff
	xoff = screenx-512
	yoff = screeny-480
	Dim As UInteger palette_address = &h3f01 + (ppu.palette * 4)
	Dim as uinteger pPalette = ppu.vram(&h3f00) + (ppu.vram(palette_address) Shl 8) + (ppu.vram(palette_address+1) Shl 16) + (ppu.vram(palette_address+2) Shl 24)
	For zz As Integer = 0 To 7
		pixel =((ppu.lbit Shr 7) and &h1) + (((ppu.ubit Shr 7) and &h1) Shl 1)
		'Line framebuffer, (xoff+((ppu.curx*2)-2),yoff+((ppu.cury*2)-1))-(xoff+((ppu.curx*2)),yoff+((ppu.cury*2)-1)), masterpalette((pPalette Shr (pixel * 8) AND &hff))
		'Line framebuffer, (xoff+((ppu.curx*2)-2),yoff+((ppu.cury*2)-2))-(xoff+((ppu.curx*2)),yoff+((ppu.cury*2)-2)), masterpalette((pPalette Shr (pixel * 8) AND &hff))
		PSet framebuffer, (xoff+ppu.curx,yoff+ppu.cury),masterpalette((pPalette Shr (pixel * 8) AND &hff))
		ppu.curx+=1
		ppu.lbit Shl = 1
		ppu.ubit Shl = 1
		If ppu.curx = 256 Then
			ppu.cury+=1
			ppu.curx=0
		EndIf
	Next
End Sub

Sub copySprites
	PPUSTATUS = PPUSTATUS And 223
	Dim As UByte sprCount = 0
	Dim As UByte sprHeight = 7
	If PPUCTRL_H Then sprHeight = 15
	For spr As UInteger = 0 To &hff Step 4
		If ((ppu.sprRAM(spr) + 1) <= ppu.scanline) And ((ppu.sprRAM(spr) + 1) + sprHeight >= ppu.scanline) Then
			If sprCount = 8 Then
				PPUSTATUS = PPUSTATUS Or 32
				Exit for
			EndIf
			For sprcopy As UByte = 0 To 3
				ppu.tempSPRram(sprCount,sprcopy) = ppu.sprRAM(spr+sprcopy)
			Next
			sprCount+=1
		EndIf
	Next
End Sub

Sub renderSprites
	'tempSPRram (0 To 7, 0 To 3) As UByte
	'for 8x8 tile number is all of byte 1, for 8, 16 its the 7 MSB
	#Define spr16Address (ppu.tempSPRram(spr,1) And 1) * &h1000
	Dim As UByte pixel
	Dim As UInteger sprTileNumber
	Dim As UInteger xoff, yoff
	Dim As UByte ubit, lbit
	Dim As UByte sprHeight = 7
	If PPUCTRL_H Then sprHeight = 15
	Dim As UInteger sprAddress
	xoff = screenx-512
	yoff = screeny-480
	For spr As UByte = 0 To 7
		If ppu.tempSPRram(spr,0) = 0 Then Exit For
		If sprHeight = 7 Then
			sprAddress = PPUCTRL_S
			sprTileNumber = ppu.tempSPRram(spr,1)
		Else
			sprAddress = spr16Address
			sprTileNumber = ppu.tempSPRram(spr,1) and &hFE ' mask out the low bit
		EndIf
		'lbit = ppu.vram((sprAddress) + (16*sprTilenumber))
		'ubit = ppu.vram((sprAddress) + ((16*sprTilenumber)+8))
		  lbit = ppu.vram((sprAddress+(ppu.scanline And sprHeight)) + (16*sprTilenumber))
        ubit = ppu.vram((sprAddress+(ppu.scanline And sprheight)) + ((16*sprTilenumber)+8))
		Dim As UInteger paletteaddr = &h3F11 + (sprTilenumber * 4)
      Dim As UInteger Ppalette = PPU.vram(&h3F00) + (PPU.vram(paletteaddr) shl 8) + (PPU.vram(paletteaddr + 1) shl 16) + (PPU.vram(paletteaddr + 2) shl 24)
		For zz As UByte = 0 To 7
      pixel =((lbit Shr 7) and &h1) + (((ubit Shr 7) and &h1) Shl 1)
      lbit Shl = 1
		ubit Shl = 1
		PSet framebuffer, (xoff+(ppu.tempSPRram(spr,3))+zz,yoff+ppu.scanline),RGB(pixel*85,pixel*85,pixel*85)
		Next
	Next
	For spr As UByte = 0 To 7
		For sprspr As UByte = 0 To 3
			ppu.tempSPRram(spr,sprspr) = 0
		Next
	Next
End Sub
Sub ppuLoop
	Select Case ppu.scanline
		Case -1 'prerender scanline
			PPUSTATUS And= &h7F
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
				copySprites
				renderSprites
			Next
		Case 240 'post render scanline
			push_framebuffer
			vblanks+=1
		Case 241 To 248
			'dsfsdfs
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
