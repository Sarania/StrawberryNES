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

Sub ProcessCurTile
	Dim As UInteger pixel
	'Dim As UInteger palette_address = &h3f00 + (ppu.curAttrb * 4)
   'dim as uinteger pPalette = cpu.memory(&h3f00) + (cpu.memory(palette_address) Shl 8) + (cpu.memory(palette_address+1) Shl 16) + (cpu.memory(palette_address+2) Shl 24)  
	For zz As Integer = 0 To 7
		pixel =((ppu.lbit Shr 7) and &h1) + ((ppu.ubit Shr 7) and &h1) Shl 1
		If pixel Then PSet framebuffer, (ppu.curx,ppu.cury), RGB(255,255,255)
		ppu.curx-=1
		ppu.lbit Shl = 1
		ppu.ubit Shl = 1
		If ppu.curx = 0 Then
			ppu.cury+=1
			ppu.curx=256
		EndIf
	Next
End Sub

Sub ppuLoop
	Select Case ppu.scanline
		Case -1 'prerender scanline
			framebuffer=ImageCreate(256,240,RGB(0,50,0))
		Case 0 To 239 'proper scanline
			ppu.tableLine = PPUCTRL_NN + ((ppu.scanline \ 8) * 32)
			ppu.attrbLine = PPUCTRL_NN + &h3C0 + ((ppu.scanline \ 32) * 8)
			For z As UByte = 0 To 31
				ppu.curAttrb = ppu.attrbLine + (z \ 4)
				ppu.curtile = ppu.tableLine+z
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
				ppu.lbit = cpu.memory(PPUCTRL_S + (ppu.curTile * 16) + (ppu.scanline and &h7))
				ppu.ubit = cpu.memory(PPUCTRL_S + (ppu.CurTile * 16) + (ppu.scanline and &h7) + 8)
				ProcessCurTile
			Next
		Case 240 'post render scanline
			Put(screenx-256,screeny-240),framebuffer,PSet
			ImageDestroy(framebuffer)
		Case Else 'vblank period
			'stuff
			ppu.curx=256
			ppu.cury=0
	End Select
	If ppu.scanline > 240 Then
		If PPUCTRL_V = 1 Then
			PPUSTATUS Or= &h80
			nmi
		EndIf
	EndIf
	ppu.scanline += 1
	If ppu.scanline = 262 Then ppu.scanline = -1
End Sub
