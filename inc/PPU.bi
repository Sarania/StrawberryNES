Declare Sub ppuLoop
Declare Sub ppuRender
Declare Sub renderBackground(ByVal z As UByte)
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
			Dim As UInteger vraddr = ppu.vraddr
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
			'ppu.addrLatch = 0
			'clear latch and scroll latch
		Case &h2004
			value = ppu.sprRAM(ppu.sprADDR)
		Case &h2007 'This section isn't working right!
			Dim As UInteger vraddr
			vraddr = ppu.vraddr
			If vraddr = &h3f10 OrElse vraddr = &h3f14 OrElse vraddr =&h3f18 OrElse vraddr = &h3f1c Then vraddr - = &h10
			If vraddr >= &h3000 AndAlso vraddr <= &h3EFF Then vraddr And = &h1000
			If vraddr >= &h3F20 AndAlso vraddr <= &h3fff Then vraddr And = &h3F1F
			If vraddr >= &h2800 AndAlso vraddr < &h3000 Then vraddr- = &h800
			value = ppu.vram(vrAddr)
		Case else
	End Select
	Return value
End Function

Sub deriveAddresses(ByVal z As UByte) 'This sub derives the addresses for the palette and the current background tile. The Z that is passed is the current background tile we are drawing for this scanline
	Dim As UInteger basePatternAddress 'This is the base address in VRAM of the pattern(it's the vram index for ppu.lbit)
	Dim As UInteger paletteOffset=4 'The amount that ppu.CurAttrb will be shifted right by
	ppu.curAttrb = ppu.vram(ppu.attrbLine + (z \ 4)) 'Select the attribute from the attribute table
	ppu.curTile = ppu.vram(ppu.tableLine+z) 'Select the tile from the pattern table
	If Not (ppu.scanline\16) And 1 Then paletteOffset = 0 'If the scanline is mod 16 then we need a base offset of 0 instead of the usual 4
	If (z\2) And 1 Then paletteOffset+ = 2 'And if z is mod 2 we need 
	ppu.paletteIndex = (((ppu.curAttrb Shr (paletteOffset)) And &h3)*4)+&h3f01 'Compute the final palette index. This is magic, don't fuck with it
	ppu.finalPalette = ppu.vram(&h3f00) + (ppu.vram(ppu.paletteIndex) Shl 8) + (ppu.vram(ppu.paletteIndex+1) Shl 16) + (ppu.vram(ppu.paletteIndex+2) Shl 24) 'More magic, don't fuck with
	basePatternAddress = (((PPUCTRL_B + (ppu.scanline AND 7)) + (ppu.curTile * 16))) 'compute the base address of the pattern in the pattern table
	ppu.lbit = ppu.vram(basePatternAddress) 
	ppu.ubit = ppu.vram(basePatternAddress+8) 'The upper part of the pattern is just 8 bytes ahead of the lower part
End Sub

Sub renderBackground(ByVal Z As UByte) 'Render the background into the PPU buffer. The Z that is passed is the current background tile we are drawing for this scanline
	For zz As Integer = 0 To 7 'Each tile is 8 pixels so we have to render 8 of them
		ppu.curx = (z*8)+zz 'compute the current Z position based on the current tile + how many pixels we've done for it
		ppu.curPixel =((ppu.lbit Shr 7) and &h1) + (((ppu.ubit Shr 7) and &h1) Shl 1) 'Derive the palette index for the current pixel
		ppuBuffer(ppu.curx,ppu.scanline) = masterpalette((ppu.finalPalette Shr (ppu.curPixel * 8) AND &hff)) 'Write the current pixel in to the PPU buffer
		ppu.lbit Shl = 1'Shift the pattern so we get the next pixel next time around
		ppu.ubit Shl = 1 'Same
	Next
End Sub

Sub copySprites 'This sub copies the 8 sprites that will be visible on this scanline into the temporary sprite RAM
   ppu.sprCount = 0 'Reset the sprite counter to 0
	If PPUCTRL_H Then ppu.sprHeight = 15 Else ppu.sprHeight = 7 'Set the sprite height based on the PPU register for it
	For spr As UInteger = 0 To 255 Step 4 'Run through the sprite ram to find what sprites will be on this scanline
		If ((ppu.sprRAM(spr)) <= ppu.scanline-1) And ((ppu.sprRAM(spr)) + ppu.sprHeight >= ppu.scanline-1) Then 'check to see if the sprite is on this scanline
			If ppu.sprCount = 8 Then 'A sprite overflow has occured
				PPUSTATUS = PPUSTATUS Or 32 'Set the sprite overflow flag
				Exit For 'And stop copying new sprites
			EndIf
			For sprcopy As UByte = 0 To 3 'Copy the sprite to the temporary sprite RAM
				ppu.tempSPRram(ppu.sprCount,sprcopy) = ppu.sprRAM(spr+sprcopy)
			Next
			ppu.sprCount+=1 'Increase the sprite count
		EndIf
	Next
End Sub

Sub renderSprites
	#Define sprPriority (ppu.tempSPRram(spr,2) And 32) / 32
	#Define spr16Address (ppu.tempSPRram(spr,1) And 1) * &h1000
	#Define palAddress (ppu.tempSPRram(spr,2) And 3)
	#Define flipY (ppu.tempSPRram(spr,2) And 128) / 128
	#Define flipX (ppu.tempSPRram(spr,2) And 64) / 64
	Dim As UInteger testPixel, pixcolor
	Dim As Byte zstart, zstop, zstep, tile16=1
	Dim As UByte pixel, ubit, lbit, sprHeight = 7
	Dim As UInteger sprTileNumber, sprAddress, paletteaddr, Ppalette
	If PPUCTRL_H Then sprHeight = 15
	For spr As Byte = 7 To 0 Step -1
		If ppu.tempSPRram(spr,0) = 0 Then GoTo skipThisOne
		If sprHeight = 7 Then
			sprAddress = PPUCTRL_S
			sprTileNumber = ppu.tempSPRram(spr,1)
		Else
			sprAddress = spr16Address
			sprTileNumber = ppu.tempSPRram(spr,1) and &hFE ' mask out the low bit
		EndIf
		If ppu.scanline - ppu.tempSPRram(spr,0) > 8 Then tile16=2
		If sprHeight = 15 And flipy = 0 Then
			If ppu.scanline - ppu.tempSPRram(spr,0) > 8 Then
				sprAddress+=8
			End If
		ElseIf sprHeight = 15 And flipy = 1 Then
			If tile16 = 1 Then sprAddress = spr16Address+8
			if tile16 = 2 then sprAddress = spr16Address
		End If

		If flipy = 1 Then
			lbit = ppu.vram((sprAddress+((sprHeight-(ppu.scanline - ppu.tempSPRram(spr,0))))) + (16*sprTilenumber)+1)
			ubit = ppu.vram((sprAddress+((sprHeight-(ppu.scanline - ppu.tempSPRram(spr,0))))) + ((16*sprTilenumber)+9))
		else
			lbit = ppu.vram((sprAddress+((ppu.scanline - ppu.tempSPRram(spr,0)))) + (16*sprTilenumber)-1)
			ubit = ppu.vram((sprAddress+((ppu.scanline - ppu.tempSPRram(spr,0)))) + ((16*sprTilenumber)+7))
		End If
		paletteaddr = &h3F11 + (palAddress * 4)
		Ppalette = PPU.vram(&h3F00) + (PPU.vram(paletteaddr) shl 8) + (PPU.vram(paletteaddr + 1) shl 16) + (PPU.vram(paletteaddr + 2) shl 24)
		If flipX Then
			zstart = 7
			zstop = 0
			zstep = -1
		Else
			zstart = 0
			zstop = 7
			zstep = 1
		EndIf
		For zz As byte = zstart To zstop Step zstep
			pixel =((lbit Shr 7) and &h1) + (((ubit Shr 7) and &h1) Shl 1)
			lbit Shl = 1
			ubit Shl = 1
			pixcolor = masterpalette((pPalette Shr (pixel * 8) AND &hff))
			'If tile16 = 1 Then pixcolor = RGB(pixel*85,0,0)
			'If tile16 = 2 Then pixcolor = RGB(0,0,pixel*85)
			testPixel = ppuBuffer((ppu.tempSPRram(spr,3)+zz),ppu.scanline)
			If pixel <> 0 Then
				If sprPriority = 0 Then
					ppuBuffer((ppu.tempSPRram(spr,3)+zz),ppu.scanline) =  pixcolor
				ElseIf sprPriority = 1 and (testpixel = -1 Or testpixel = 0) Then
					ppuBuffer((ppu.tempSPRram(spr,3)+zz),ppu.scanline) = pixcolor
				EndIf
				If (spr = 0) And (testpixel <> -1) And (spritehit = 0) Then
					ppuBuffer((ppu.tempSPRram(spr,3)+zz),ppu.scanline) = pixcolor
					PPUSTATUS Or= &H40
					spritehit = 1
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
	Dim As UInteger xoff = screenx - (256*sf)
	Dim As UInteger yoff = screeny - (240*sf)
	For yyy As Integer = 0 To 239
		For xxx As Integer = 0 To 255
			For zzz As Integer = 0 To sf-1
				If ppubuffer(xxx,yyy) <>  oldbuffer(xxx,yyy) Andalso ppubuffer(xxx,yyy) <> -1 Then
					Line nesbuffer, (xoff+(xxx*sf-sf),yoff+(yyy*sf-zzz))-(xoff+(xxx*sf),yoff+(yyy*sf-zzz)), ppubuffer(xxx,yyy)
				ElseIf forcerender = 1 Then
					Line nesbuffer, (xoff+(xxx*sf-sf),yoff+(yyy*sf-zzz))-(xoff+(xxx*sf),yoff+(yyy*sf-zzz)), ppubuffer(xxx,yyy)
					forcerender = 0
				End if
			Next
		Next
	Next
	Put framebuffer,(0,0),nesbuffer, trans
End Sub
Sub ppuLoop
	ppu.curx = 0
	Select Case ppu.scanline
		Case -1 'prerender scanline
			PPUSTATUS And= &h7F 'clear vblank flag
			PPUSTATUS And= &hBF ' This clears the sprite zero bit
			spritehit = 0
		Case 0 To 239 'proper scanline
			ppu.tableLine = PPUCTRL_NN + ((ppu.scanline \ 8) * 32)
			ppu.attrbLine = PPUCTRL_NN + &h3C0 + ((ppu.scanline \ 32) * 8)
			For z As UByte = 0 To 31
				deriveAddresses(z) 'This sub derives the nametable and palette addresses for the background
				If ppumask_b = 1 Then renderBackground(z) 'This sub renders the background in to the framebuffer array
				copySprites 'This sub copies the sprites for the current scanline from main sprite memory to temporary sprite memory
				If ppumask_s = 1 Then renderSprites 'This sub renders the sprites in to the framebuffer array
			next
		Case 240 'post render scanline
			ppuRender 'This sub renders the framebuffer array in to the main framebuffer while scaling according to the scalefactor
			push_framebuffer 'This sub pushes the framebuffer to the screen
			vblanks+=1
		Case 241 To 262
			If ppu.scanline = 241 Then ppustatus or=&h80 'set vblank flag
			If ppu.scanline = 241 And PPUCTRL_V = 1 Then	nmi
			ppu.curx=0
		Case Else 'shouldn't come here!
			beep
	End Select
	ppu.scanline += 1
	If ppu.scanline = 262 Then ppu.scanline = -1
End Sub
