Declare Sub ppuLoop
Declare Sub ppuRender
Declare Sub renderBackground(ByVal z As UByte)
Declare Sub copySprites
Declare Sub renderSprites
Declare Sub deriveAddresses(ByVal z As UByte)
Declare Function readPPUreg(ByVal addr As UShort)As Ubyte
Declare sub writePPUreg(ByVal addr As UShort, ByVal value As UByte)
Dim Shared As UByte readBuff
Dim Shared As UInteger linemax
Dim Shared As UByte ptrX, ptrY, tabSel
Sub writePPUreg(ByVal addr As UShort, ByVal value As UByte)
	Select Case(addr)
		Case &h2000
			PPUCTRL = value
			value And= &b00000011
			Dim As UShort tvalue = value Shl 10
			ppu.tempaddr And = &b0111001111111111
			ppu.tempaddr Or= tvalue
		Case &h2001
			PPUMASK = value
		Case &h2003
			ppu.sprAddr = (value) 'Or (ppu.sprAddr Shl 8)
		Case &h2004
			ppu.sprRAM(ppu.sprAddr) = value
			ppu.sprAddr +=1
		Case &h2005
			If ppu.addrLatch = 0 Then 'first write
				ppu.addrLatch = 1
				ppu.xTile = value Shr 3
				ppu.xFine = value And &b00000111
				Dim As UShort tvalue = value Shr 3
				ppu.tempaddr And= &b0111111111100000
				ppu.tempaddr Or= tvalue
				tvalue = value
				tvalue Shl= 8
				PPUSCROLL And= &b0000000011111111
				PPUSCROLL Or= tvalue
			Else
				ppu.addrLatch = 0 'second write
				ppu.yTile = value Shr 3
				ppu.yFine = value And &b00000111
				ppu.tempaddr And= &b0000110000011111
				Dim As UShort tvalue = ppu.yFine Shl 12
				ppu.tempaddr Or= tvalue
				tvalue = ppu.yTile Shl 5
				ppu.tempaddr Or= tvalue
				tvalue = value
				PPUSCROLL And= &b1111111100000000
				PPUSCROLL Or= tvalue
			EndIf

		Case &h2006
			If ppu.addrLatch = 0 Then 'first write
				ppu.addrLatch = 1
				ppu.tempaddr And= &b0000000011111111
				Dim As UShort tvalue = value
				tvalue = value Shl 8
				ppu.tempaddr Or = tvalue
				ppu.tempaddr And= &b0011111111111111
				PPUADDR And= &b0000000011111111
				PPUADDR Or= tvalue
			Else
				ppu.addrLatch = 0 'second write
				ppu.tempaddr And= &b0111111100000000
				Dim As UShort tvalue = value
				ppu.tempaddr Or= tvalue
				ppu.vrAddr = ppu.tempaddr
				PPUADDR And= &b1111111100000000
				PPUADDR Or= tvalue
			EndIf
		Case &h2007
			Dim As UInteger vraddr = ppu.vraddr

			'	If vraddr >= &h2400 AndAlso vraddr < &h27FF Then vraddr -=&h400
			'	If vraddr >= &h2c00 AndAlso vraddr < &h2FFF Then vraddr -=&h400
			'ElseIf mirroring = "V" Then
			'	If vraddr >= &h2800 AndAlso vraddr < &h2BFF Then vraddr -=&h800
			'	If vraddr >= &h2c00 AndAlso vraddr < &h2FFF Then vraddr -=&h800
			'EndIf
			*ppu.vram(vrAddr) = value
			If PPUCTRL_I = 1 Then
				ppu.vrAddr += 32
			Else ppu.vrAddr += 1
			EndIf
			ppu.vrAddr And= &h3FFF
		Case &h4014
			Dim As UShort dmaaddr = value * &H100
			For i As UInteger = 0 To &hFF
				ppu.sprRAM(i) = cpu.memory(dmaaddr+i)
			Next
	End Select
End Sub

Function readPPUreg(ByVal addr As UShort)As UByte
	If instruction = "LDA" OrElse instruction ="LDX" OrElse instruction = "LDY" OrElse instruction = "CMP" OrElse instruction = "BIT"  Then
		Dim As UByte value
		Select Case addr
			Case &h2002
				value = PPUSTATUS
				PPUSTATUS = PPUSTATUS And &h7F
				ppu.addrLatch = 0 'clear latch and scroll latch
			Case &h2004
				Beep
				value = ppu.sprRAM(ppu.sprADDR)
				If Not (ppustatus And &h80) Then  ppu.sprADDR+=1
			Case &h2007 'This section isn't working right!

				Dim As UInteger vraddr
				vraddr = ppu.vraddr
				If vraddr = &h3f10 OrElse vraddr = &h3f14 OrElse vraddr =&h3f18 OrElse vraddr = &h3f1c Then vraddr - = &h10
				If vraddr >= &h3000 AndAlso vraddr <= &h3EFF Then vraddr And = &h1000
				If vraddr >= &h3F20 AndAlso vraddr <= &h3fff Then vraddr And = &h3F1F
				If vraddr >= &h2800 AndAlso vraddr < &h3000 Then vraddr- = &h800
				If mirroring = "H" Then
					If vraddr >= &h2400 AndAlso vraddr < &h27FF Then vraddr -=&h400
					If vraddr >= &h2c00 AndAlso vraddr < &h2FFF Then vraddr -=&h400
				ElseIf mirroring = "V" Then
					If vraddr >= &h2800 AndAlso vraddr < &h2BFF Then vraddr -=&h800
					If vraddr >= &h2c00 AndAlso vraddr < &h2FFF Then vraddr -=&h800
				EndIf
				If ppu.vrAddr < &h3f00 Then
					value = readBuff
					readBuff = *ppu.vram(ppu.vrAddr )
				Else
					value = *ppu.vram(ppu.vrAddr)
				EndIf

				If PPUCTRL_I = 1 Then
					ppu.vrAddr += 32
				Else ppu.vrAddr += 1
				EndIf
				ppu.vrAddr And= &h3FFF


			Case Else


		End Select
		Return value
	EndIf
End Function

Sub deriveAddresses(ByVal z As UByte) 'This sub derives the addresses for the palette and the current background tile. The Z that is passed is the current background tile we are drawing for this scanline
	Dim As UInteger paletteOffset=4 'The amount that ppu.CurAttrb will be shifted right by
	Dim As UByte ytmp = ptry+ppu.yTile
	Dim As UByte xtmp = z+ppu.xTile+tabSel
	If xtmp > 63 Then xtmp-=64
	If ytmp > 59 Then ytmp-=60
	ppu.curTile = *nameTable(xtmp,(ytmp),0)
	ppu.curAttrb = *nameTable(xtmp,(ytmp),1)
	If Not ((ppu.scanline+(ppu.yTile)*8)\16) And 1 Then paletteOffset = 0 'If the scanline is mod 16 then we need a base offset of 0 instead of the usual 4
	If ((z+ppu.xTile)\2) And 1 Then paletteOffset+ = 2 'And if z is mod 2 we need
	ppu.paletteIndex = (((ppu.curAttrb Shr (paletteOffset)) And &h3)*4)+&h3f01 'Compute the final palette index. This is magic, don't fuck with it
	ppu.finalPalette = *ppu.vram(&h3f00) + (*ppu.vram(ppu.paletteIndex) Shl 8) + (*ppu.vram(ppu.paletteIndex+1) Shl 16) + (*ppu.vram(ppu.paletteIndex+2) Shl 24) 'More magic, don't fuck with
	ppu.basePatternAddress = (((PPUCTRL_B + (ppu.scanline And 7)) + (ppu.curTile * 16))) 'compute the base address of the pattern in the pattern table
	ppu.lbit = *ppu.vram(ppu.basePatternAddress)
	ppu.ubit = *ppu.vram(ppu.basePatternAddress+8) 'The upper part of the pattern is just 8 bytes ahead of the lower part
End Sub

Sub renderBackground(ByVal Z As UByte) 'Render the background into the PPU buffer. The Z that is passed is the current background tile we are drawing for this scanline
	Dim As UInteger curx
	Dim As Integer tmpx,tmpy
	For zz As Integer = 0 To 7 'Each tile is 8 pixels so we have to render 8 of them
		curx = (z*8)+zz 'compute the current Z position based on the current tile + how many pixels we've done for it
		tmpx = curx - ppu.xFine
		tmpy = ppu.scanline - ppu.yFine
		If tmpx < 0 Then tmpx = 0
		If tmpy < 0 Then tmpy = 0
		ppu.curPixel =((ppu.lbit Shr 7) And &h1) + (((ppu.ubit Shr 7) And &h1) Shl 1) 'Derive the palette index for the current pixel
		ppuBuffer(tmpx,tmpy) = (ppu.finalPalette Shr (ppu.curPixel * 8) And &hff) 'Write the current pixel in to the PPU buffer
		If ppu.curpixel <> 0 Then backIsTransparent = 0
		backBuffer(tmpx,tmpy) = ppu.curPixel
		ppu.lbit Shl = 1'Shift the pattern so we get the next pixel next time around
		ppu.ubit Shl = 1 'Same
	Next
End Sub

Sub copySprites 'This sub copies the 8 sprites that will be visible on this scanline into the temporary sprite RAM
	ppu.sprCount = 0 'Reset the sprite counter to 0
	If PPUCTRL_H Then ppu.sprHeight = 15 Else ppu.sprHeight = 7 'Set the sprite height based on the PPU register for it
	For spr As UInteger = 0 To 255 Step 4 'Run through the sprite ram to find what sprites will be on this scanline
		If ((ppu.sprRAM(spr)) +1 <= ppu.scanline) AndAlso ((ppu.sprRAM(spr)) + ppu.sprHeight +1  >= ppu.scanline) Then 'check to see if the sprite is on this scanline
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
	Dim As Byte zstart, zstop, zstep 'These vars are used for X flipping. The for loop is 0-7 step 1 when not flipped and 7-0 step -1 when flipped
	Dim As UInteger testPixel, pixcolor
	If PPUCTRL_H Then ppu.sprHeight = 15 Else ppu.sprHeight = 7 'Set sprite height based on value in the register
	For spr As Byte = 0 To 7 Step 1 'There are up to 8 sprites per scanp
		If ppu.tempSPRram(spr,0) = 0 Then GoTo skipThisOne 'If the Y value is 0 then there is no sprite so skip the blank spr
		If ppu.sprHeight = 7 Then 'The sprite addresses are computed slightly differently depending on the height
			ppu.sprAddress = PPUCTRL_S
			ppu.sprTileNumber = ppu.tempSPRram(spr,1)
		ElseIf ppu.sprHeight = 15 Then
			ppu.sprAddress = spr16Address 'spr16Address is a macro that returns the correct base address
			ppu.sprTileNumber = ppu.tempSPRram(spr,1) And &hFE 'Mask out the low bit to return the correct tile number
			If flipy = 0 Then
				If ppu.scanline - ppu.tempSPRram(spr,0) > 8 Then 'We are on the second tile which is 8 bytes ahead of the first
					ppu.sprAddress+=8
				EndIf
			Else 'Flipped 8x16 sprites are a pain. This handles it!
				If ppu.scanline - ppu.tempSPRram(spr,0) > 8 Then ppu.sprAddress = spr16Address Else ppu.sprAddress = spr16Address+8
			EndIf
		EndIf
		If flipX = 1 Then 'Reverse the for loop if the sprite is flipped horizontally
			zstart = 7
			zstop = 0
			zstep = -1
		Else 'Not flipped so do it normally
			zstart = 0
			zstop = 7
			zstep = 1
		EndIf

		If flipy = 1 Then 'Reverse the order of bits for vertically flipped sprites
			ppu.basePatternAddress = (ppu.sprAddress+((ppu.sprHeight-(ppu.scanline - ppu.tempSPRram(spr,0))))) + (16*ppu.sprTilenumber)+1
		Else 'not flipped so do it normally
			ppu.basePatternAddress = (ppu.sprAddress+((ppu.scanline - ppu.tempSPRram(spr,0)))) + (16*ppu.sprTilenumber)-1
		End If
		ppu.lbit = *ppu.vram(ppu.basePatternAddress)
		ppu.ubit = *ppu.vram(ppu.basePatternAddress+8) 'in all cases the second part of the pattern is 8 bytes ahead of the first
		ppu.paletteindex = &h3F11 + (sprpalAddress * 4) 'Palette magic, do not fuck with
		ppu.finalPalette = *ppu.vram(&h3F00) + (*ppu.vram(ppu.paletteindex) Shl 8) + (*ppu.vram(ppu.paletteindex + 1) Shl 16) + (*ppu.vram(ppu.paletteindex + 2) Shl 24) 'More palette magic to not fuck with
		For zz As Byte = zstart To zstop Step zstep
			ppu.curpixel =((ppu.lbit Shr 7) And &h1) + (((ppu.ubit Shr 7) And &h1) Shl 1) 'Get the pattern index of the current pixel
			ppu.lbit Shl = 1 'Shift the pixels so we get the right one next time
			ppu.ubit Shl = 1
			pixcolor = (ppu.finalPalette Shr (ppu.curpixel * 8)) And &hff 'More palette magic, do not fuck with
			testPixel = backbuffer((ppu.tempSPRram(spr,3)+zz),ppu.scanline) 'The pixel from the background for testing sprite priority and spr0
			If spr = 0  Then 'This is sprite 0, check for a hit
				If spritehit = 0 Then 'spr0 only hits once per scanline
					If (ppu.tempSPRram(spr,3)+zz > 7) AndAlso (ppu.tempSPRram(spr,3)+zz <> 255) Then 'spr0 doesn't hit on 0-7 and 255
						If (ppumask_b = 1) AndAlso (ppumask_s = 1) Then 'spr0 doesn't hit if EITHER the background or sprite rendering is disabled
							If (ppu.curpixel <> 0) Then 'spr0 only hits when BOTH the background and the sprite are not transparent
								If backisTransparent = 0 Then
									spritehit = 1 'This keeps track of whether we had a spr0 this frame yet
									ppustatus Or= &h40 'Set the PPU flag
								End If
							EndIf
						EndIf
					EndIf
				EndIf
			End If
			If sprPriority = 0 Then 'Sprite priority is set, this sprite is in front
				If ppu.curpixel <> 0 Then sprbuffer((ppu.tempSPRram(spr,3)+zz),ppu.scanline) =  pixcolor
			ElseIf sprPriority = 1 Then 'Sprite priority is off, this sprite is behind
				If testpixel = 0 Then sprbuffer((ppu.tempSPRram(spr,3)+zz),ppu.scanline) =  pixcolor
			EndIf
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
				If sprbuffer(XXX,yyy) <> 0 Then ppubuffer(xxx,yyy) = sprbuffer(xxx,yyy)
				sprbuffer(xxx,yyy) = 0
				If ppubuffer(xxx,yyy) <>  oldbuffer(xxx,yyy) AndAlso ppubuffer(xxx,yyy) <> -1 Then
					Line nesbuffer, (xoff+(xxx*sf-sf),yoff+(yyy*sf-zzz))-(xoff+(xxx*sf),yoff+(yyy*sf-zzz)), masterpalette(ppubuffer(xxx,yyy))
				ElseIf forcerender = 1 Then
					Line nesbuffer, (xoff+(xxx*sf-sf),yoff+(yyy*sf-zzz))-(xoff+(xxx*sf),yoff+(yyy*sf-zzz)), masterpalette(ppubuffer(xxx,yyy))
					forcerender = 0
				End If
			Next
		Next
	Next
	Put framebuffer,(0,0),nesbuffer, Trans
End Sub
Sub ppuLoop
	Select Case ppu.scanline
		Case -1 'prerender scanline
			backIsTransparent = 1
			PPUSTATUS And= &h7F 'clear vblank flag
			PPUSTATUS And= &hBF ' This clears the sprite zero bit
			spritehit = 0

		Case 0 To 239 'proper scanline

			If ppuctrl_nn <> &h2000 Then tabSel = 32 Else tabSel = 0
			'If spritehit = 0 Then tabsel=0
			ptrY= ppu.scanline \ 8
			For z As UByte = 0 To 32
				deriveAddresses(z) 'This sub derives the nametable and palette addresses for the background
				If ppumask_b = 1 Then renderBackground(z) 'This sub renders the background in to the framebuffer array
				If z = 32 Then
					copySprites 'This sub copies the sprites for the current scanline from main sprite memory to temporary sprite memory
					If ppumask_s = 1 Then renderSprites 'This sub renders the sprites in to the framebuffer arra
				End If
			Next
		Case 240 'post render scanline
			ppuRender 'This sub renders the framebuffer array in to the main framebuffer while scaling according to the scalefactor
			push_framebuffer 'This sub pushes the framebuffer to the screen
			vblanks+=1
		Case 241 To 262
			If ppu.scanline = 241 Then ppustatus Or=&h80 'set vblank flag
			If ppu.scanline = 241 And PPUCTRL_V = 1 Then	nmi
		Case Else 'shouldn't come here!
			Beep
	End Select
	ppu.scanline += 1
	If ppu.scanline = 262 Then ppu.scanline = -1
End Sub
