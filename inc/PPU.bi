Type ppus
	VRAM(0 To &h1000) As UByte 'Video RAM
	SPR(0 To &H100) As UByte '256 Byte Sprite Attribute RAM
End Type
Dim As UShort scanline
Dim As UShort lines = 240 
Dim As UShort baseNameTable
Dim As UShort baseTile
dim as ubyte  xScroll
dim as ubyte  yScroll
Dim Shared ppu As ppus
Declare Sub writePPUReg(ByVal value As UByte, ByVal addr As UShort)
Declare Sub readPPUReg(ByVal value As UByte)
Declare Sub vblank
Declare Sub updateDrawCrap
Declare Sub drawPix
Declare Sub ppuLoop

Sub vblank
	cpu.memory(&h2002) Or= &h80 'Set Vbl Flag in PPU STATUS
	 Then If Bit(cpu.memory(&h2000),7) = 1 Then cpu.pc = (cpu.memory(&hFFFB) Shl 8) Or cpu.memory(&hFFFA) 'Are NMIs Enabled? VECTOR!
End Sub
Sub writePPUReg(ByVal value As UByte, ByVal addr As UShort)
Dim As UByte scrollFlop 'Flip Flop for SCROLL
Dim As UByte addrFlop 	'Flip Flop for PPUADDR
Dim As UShort ppuAddr	'PPU Address
	Select Case(addr & &h7)
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
	If scrollFlop = 0 Then xScroll = value Else yScroll = value
	flipFlop +=1
	cpu.memory(&h2005) = value
	Case &h2006 'PPUADDR
	'2X
	If addrFlop > 1 Then addrFlop = 0 
	If addrFlop = 0 Then ppuAddr = (value Shr 8)
	If addrFlop = 1 Then ppuAddr Or= value 
	cpu.memory(&h2006) = value
	Case &h2007 'PPUDATA
	cpu.memory(&h2007) = value
	Case &h4014
End Sub

Sub readPPUReg(ByVal value As UByte)
	Select Case 
		'Only a few registers can be read
		Case 2
		value = cpu.memory(&h2002) 
		BitReset(cpu.memory(&h2002),7) 'Clear VBL Flag on Read
		
		Case 4
			
		Case 7 
			
		
	End Select
	Return value
End Sub
Sub updateDrawCrap
	'0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00 
	baseNameTable = (cpu.memory(&h2000)And 1)*&h400
	'0 = $0000; 1 = $1000;
	baseTile = ((cpu.memory(&h2000)Shr 1)And 1)*&h1000
End Sub
Sub drawPix
	
End Sub
Sub ppuLoop
	If scanline = lines Then vblank
End Sub
