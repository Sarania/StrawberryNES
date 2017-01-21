Type ppus
	background(0 To 255, 0 To 240) As ubyte
	VRAM(0 To &h1000) As UByte 'Video RAM
	SPR(0 To &H100) As UByte '256 Byte Sprite Attribute RAM
	xScroll As UByte
	yScroll As UByte
	baseTile As UShort
	baseNameTable As UShort 
	scanline As UByte
	lines As UByte
	vScan As UByte 
End Type
Dim As UShort lines = 240 
Dim Shared ppu As ppus
Declare Sub writePPUReg(ByVal value As UByte, ByVal addr As UShort)
Declare function readPPUReg(ByVal value As UByte) As UShort
Declare Sub vblank
Declare Sub updateDrawCrap
Declare Sub drawPix
Declare Sub ppuLoop

Sub vblank

End Sub
Sub writePPUReg(ByVal value As UByte, ByVal addr As UShort)
Dim As UByte scrollFlop 'Flip Flop for SCROLL
Dim As UByte addrFlop 	'Flip Flop for PPUADDR
Dim As UShort ppuAddr	'PPU Address
	Select Case(addr and &h2007)
	Case &h2000 'PPUCTRL
	cpu.memory(&h2000) = value
	Case &h2001 'PPUMASK
	cpu.memory(&h2001) = value
	Case &h2003 'OAMADDR
	cpu.memory(&h2003) = value
	Case &h2004 'OAMDATA
	cpu.memory(&h2004) = value
	Case &h2005 'PPUSCROLL
	'2X
	If scrollFlop > 1 Then scrollFlop = 0 'Catch the correct write
	If scrollFlop = 0 Then ppu.xScroll = value Else ppu.yScroll = value
	scrollFlop +=1
	cpu.memory(&h2005) = value
	Case &h2006 'PPUADDR
	'2X
	If addrFlop > 1 Then addrFlop = 0 
	If addrFlop = 0 Then ppuAddr = (value Shr 8)
	If addrFlop = 1 Then ppuAddr Or= value 
	cpu.memory(&h2006) = value
	Case &h2007 'PPUDATA
	cpu.memory(&h2007) = value
	End select
End Sub

Function readPPUReg(ByVal addr As UByte)As ushort
	Dim As UByte value 
	Select Case (addr And &h2007)
		'Only a few registers can be read
		Case 2
		value = cpu.memory(&h2002) 
		cpu.memory(&h2002) = BitReset(cpu.memory(&h2002),7) 'Clear VBL Flag on Read
		Print "Reading PPU STATUS "
		cpu.memory(&h500) = &he
		Case 4
		'Need to work on this. Too lazy now. 
		Case 7 
			
		
	End Select
	Return value
End Function
Sub drawScreen
	/'0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00 
	ppu.baseNameTable = (cpu.memory(&h2000)And 1)*&h400
	'0 = $0000; 1 = $1000;
	ppu.baseTile = (ppu.VRAM(ppu.VRAM(ppu.baseNameTable)+ppu.xScroll))
	'/
End Sub
Sub drawPix
	
	
End Sub
Sub ppuLoop
	Dim As UByte dot
	If ppu.scanline > 240 Then dot = 255 Else dot = 0 
	Do
		
	dot += 1
	
	
	
	
	Loop Until dot = 255
'''''''''''''''''''''''''''''''''''''''''''''''''''''


End Sub
