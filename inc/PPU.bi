Declare Sub ppuLoop
Declare Sub ppuRender
Declare Sub renderBackground
Declare Sub copySprites
Declare Sub renderSprites
Declare Sub deriveAddresses(ByVal z As UByte)
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
			Dim As UInteger vraddr
			vraddr = ppu.vraddr
			If vraddr = &h3f10 OrElse vraddr = &h3f14 OrElse vraddr =&h3f18 OrElse vraddr = &h3f1c Then vraddr - = &h10
			If vraddr >= &h3000 AndAlso vraddr <= &h3EFF Then vraddr And = &h1000
			If vraddr >= &h3F20 AndAlso vraddr <= &h3fff Then vraddr And = &h3F1F
			'If vraddr >= &h2800 AndAlso vraddr < &h3000 Then vraddr- = &h800
			ppu.vram(vrAddr) = value
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
			Dim As UInteger vraddr
			vraddr = ppu.vraddr
			If vraddr = &h3f10 OrElse vraddr = &h3f14 OrElse vraddr =&h3f18 OrElse vraddr = &h3f1c Then vraddr - = &h10
			If vraddr >= &h3000 AndAlso vraddr <= &h3EFF Then vraddr And = &h1000
			If vraddr >= &h3F20 AndAlso vraddr <= &h3fff Then vraddr And = &h3F1F
			'If vraddr >= &h2800 AndAlso vraddr < &h3000 Then vraddr- = &h800
			value = ppu.vram(vrAddr)
		Case else
			'do
	End Select
	Return value
End Function

Sub deriveAddresses(ByVal z As UByte)
	Dim temp_P As UInteger
	ppu.curAttrb = ppu.vram(ppu.attrbLine + (z \ 4))
	ppu.curtile = ppu.vram(ppu.tableLine+z)
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
End Sub

Sub renderBackground
	Dim As Ubyte pixel
	Dim As UInteger xoff, yoff
	xoff = screenx-512
	yoff = screeny-480
	Dim As UInteger palette_address = &h3f01 + (ppu.palette * 4)
	Dim as uinteger pPalette = ppu.vram(&h3f00) + (ppu.vram(palette_address) Shl 8) + (ppu.vram(palette_address+1) Shl 16) + (ppu.vram(palette_address+2) Shl 24)
	For zz As Integer = 0 To 7
		pixel =((ppu.lbit Shr 7) and &h1) + (((ppu.ubit Shr 7) and &h1) Shl 1)
		ppuBuffer(ppu.curx,ppu.scanline) = masterpalette((pPalette Shr (pixel * 8) AND &hff))
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
				msg = "SPR overflow on SL " & str(ppu.scanline)
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
	#Define sprPriority (ppu.tempSPRram(spr,2) And 32) / 32
	#Define spr16Address (ppu.tempSPRram(spr,1) And 1) * &h1000
	#Define palAddress (ppu.tempSPRram(spr,2) And 3)
	#Define flipY (ppu.tempSPRram(spr,2) And 128)
	#Define flipX (ppu.tempSPRram(spr,2) And 64)
	Dim As UInteger testPixel, pixcolor
	Dim As Byte zstart, zstop, zstep, zoff, zyoff
	Dim As UByte pixel, ubit, lbit, sprHeight = 7
	Dim As UInteger sprTileNumber, xoff, yoff, sprAddress, paletteaddr, Ppalette
	If PPUCTRL_H Then sprHeight = 15
	xoff = screenx-512
	yoff = screeny-480
	For spr As Byte = 7 To 0 Step -1
		If ppu.tempSPRram(spr,0) = 0 Then GoTo skipThisOne
		If sprHeight = 7 Then
			sprAddress = PPUCTRL_S
			sprTileNumber = ppu.tempSPRram(spr,1)
		Else
			sprAddress = spr16Address
			sprTileNumber = ppu.tempSPRram(spr,1) and &hFE ' mask out the low bit
		EndIf
		If flipY Then
			lbit = ppu.vram((sprAddress+((7-(ppu.scanline - ppu.tempSPRram(spr,0))))) + (16*sprTilenumber)+1)
			ubit = ppu.vram((sprAddress+((7-(ppu.scanline - ppu.tempSPRram(spr,0))))) + ((16*sprTilenumber)+9))
		Else
			lbit = ppu.vram((sprAddress+((ppu.scanline - ppu.tempSPRram(spr,0)))) + (16*sprTilenumber)-1)
			ubit = ppu.vram((sprAddress+((ppu.scanline - ppu.tempSPRram(spr,0)))) + ((16*sprTilenumber)+7))
		End If
		paletteaddr = &h3F11 + (palAddress * 4)
		Ppalette = PPU.vram(&h3F00) + (PPU.vram(paletteaddr) shl 8) + (PPU.vram(paletteaddr + 1) shl 16) + (PPU.vram(paletteaddr + 2) shl 24)
		If flipX Then
			zstart = 7
			zstop = 0
			zstep = -1
			zoff = -1
		Else
			zstart = 0
			zstop = 7
			zstep = 1
			zoff = 0
		EndIf
		For zz As byte = zstart To zstop Step zstep
			pixel =((lbit Shr 7) and &h1) + (((ubit Shr 7) and &h1) Shl 1)
			lbit Shl = 1
			ubit Shl = 1
			pixcolor = masterpalette((pPalette Shr (pixel * 8) AND &hff))
			testPixel = ppuBuffer(ppu.tempSPRram(spr,3)+zz,ppu.scanline)
			If pixel <> 0 Then
				If sprPriority = 0 Then
					ppuBuffer(ppu.tempSPRram(spr,3)+zz,ppu.scanline) =  pixcolor
				ElseIf sprPriority = 1 and (testpixel = -1 Or testpixel = 0) Then
					ppuBuffer((ppu.tempSPRram(spr,3)+zz),ppu.scanline) = pixcolor
				EndIf
				If spr = 0 And testpixel <> -1 Then
					ppuBuffer((ppu.tempSPRram(spr,3)+zz),ppu.scanline) = pixcolor
					PPUSTATUS Or= &H40
				EndIf
			End If
		Next
		
		skipthisOne:
	Next
	For spr As UByte = 0 To 7
		For sprspr As UByte = 0 To 3
			ppu.tempSPRram(spr,sprspr) = 0
		Next
	Next
End Sub

Sub ppuRender
	Dim As UByte sf = 2
	Dim As UInteger xoff = screenx - (256*sf)
	Dim As UInteger yoff = screeny - (240*sf)
	For yyy As Integer = 0 To 239
		For xxx As Integer = 0 To 255
			For zzz As Integer = 0 To sf-1
				If ppubuffer(xxx,yyy) <> 0 AndAlso ppubuffer(xxx,yyy) <> -1 Then Line framebuffer, (xoff+(xxx*sf-sf),yoff+(yyy*sf-zzz))-(xoff+(xxx*sf),yoff+(yyy*sf-zzz)), ppubuffer(xxx,yyy)
			Next
		Next
	Next
End Sub
Sub ppuLoop
	Select Case ppu.scanline
		Case -1 'prerender scanline
			PPUSTATUS And= &h7F 'clear vblank flag
			PPUSTATUS And= &hBF ' This clears the sprite zero bit
		Case 0 To 239 'proper scanline
			ppu.tableLine = PPUCTRL_NN + ((ppu.scanline \ 8) * 32)
			ppu.attrbLine = PPUCTRL_NN + &h3C0 + ((ppu.scanline \ 32) * 8)
			For z As UByte = 0 To 31
				deriveAddresses(z) 'This sub derives the nametable and palette addresses for the background
				renderBackground 'This sub renders the background in to the framebuffer array
				copySprites 'This sub copies the sprites for the current scanline from main sprite memory to temporary sprite memory
				renderSprites 'This sub renders the sprites in to the framebuffer array
			next
		Case 240 'post render scanline
			ppuRender 'This sub renders the framebuffer array in to the main framebuffer while scaling according to the scalefactor
			push_framebuffer 'This sub pushes the framebuffer to the screen
			vblanks+=1
		Case 241 To 248
			'dsfsdfs
			If ppu.scanline = 247 Then
				If PPUCTRL_V = 1 Then
					nmi
				EndIf
			EndIf
		Case Else 'vblank period
			'stuff
			ppustatus or=&h80 'set vblank flag
			ppu.curx=0
			ppu.cury=0
	End Select
	ppu.scanline += 1
	If ppu.scanline = 262 Then ppu.scanline = -1
End Sub
