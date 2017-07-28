/'6502 emulator written in FreeBasic

License and Disclaimer

This is my license and disclaimer document. By downloading and using any of my software you agree to be bound by it.
This document supercedes any other license and disclaimer I may have made prior to its writing.


License:

    * The software is free, and you may use it as long as you like.
    * You may redistribute the software, as long as this license is included.
    * The source code to the software is included, you may modify it, improve it etc.
    * You may make derivative works.
    * Any derivative works you make must also include this license and disclaimer
    * I respectfully request but do not require that should you improve the software or make
	 a derivative work, you notify me of it. My contact information is generally included
	in the program's source. (Blyss.Sarania@Gmail.com)
.
I am pretty permissive with my stuff, but there ARE some things you can not do with it:

    * You may NOT sell the software, in whole or part.
    * You may NOT use the software to engage in or promote copyright infringement of any kind.
    * You may NOT use the software to cause offense to, insult, defame, hurt or harass any person or corporate entity.
    * You may NOT modify this license if you redistribute the software, whether you made changes to the application or not.
    * You may NOT remove any current or previous contributors credit from the source or documentation,
	even if you negated their work.


Disclaimer:
This software is provided AS IS without any warranty, including the implied ones for merchantability and/or fitness
for a particular purpose. Use of any of the software is AT YOUR OWN RISK. You agree that should any
negative effect result from the use of the software, you are solely responsible.

________________________

This document last updated at: 19:21 CST 3/28/2014

Copyright 2014 Blyss Sarania
'/
#Define debugmode
Randomize Timer
#Include Once "fbgfx.bi"
Using fb
#Include Once "string.bi"
#Include Once "crt.bi" 'c runtime
#Include Once "file.bi" 'File functions
#Include Once "Freeimage.bi" ' Freeimage library
#Include Once "inc/freetofb.bi" 'Easily use Freeimage images in Freebasic
#Include Once "zlib.bi" 'This is needed for the Freeimage library
Declare Function readmem(ByVal addr As ULongInt, ByVal numbytes As UInteger = 1) As Ushort ' for reading memory
Declare Sub writemem(ByVal addr As ULongInt, ByVal value As UByte) 'write a value to NES memory
Declare Sub status 'print various status stuff to the screen
Declare Sub initcpu 'initialize the 6502
Declare Sub loadROM 'load a ROM in to memory
Declare Sub CAE 'Cleanup and exit
Declare Sub loadini 'Load the ini file
Declare Sub writeini
Declare Sub options
Declare Sub romInfo
Declare Sub savestate
Declare Sub loadstate
Declare Sub nmi
#ifdef debugmode
'======================================================ONLY INCLUDED IF DEBUGMODE IS DEFINED!======================================================================
Dim Shared As String curline
Dim Shared As String opdata, opc
Dim Shared As ULongInt nintcyc
Dim Shared As UInteger nintsl
Dim Shared As UByte logcomp = 1
Declare Sub write_the_log
Declare Sub comparelog
Declare Sub fail(ByVal op As String, ByVal expected As String, ByVal actual As String)
'=======================7===========================================================================================================================================
#EndIf
Declare Sub push_framebuffer
Declare Sub clear_framebuffer
Declare Sub frameLimit
Dim Shared As Any Ptr nesbuffer
Dim Shared As UByte debug, mapper, spritehit = 0, trace_done = 0, flimit = 1, sf = 2, forcerender = 0, do_trace = 1, backIsTransparent = 1, fullscreen = 0, fitwindow = 0, saveSlot = 1
Dim Shared As UInteger opstoskip, nextskip, opGoal, romsize, screenx, screeny, centerx, centery, totalops, ticksPerSecond, logops=0
Dim Shared As String opHistory(0 To 255), emulatorMode, instruction, amode, msg, version, lastrom, shpname, gamename, mirroring, TVsystem
Dim Shared As Single start, lastframetime,opsPerSecond, stepstart,fps, vstart, curtime
Dim Shared As Any Ptr strawberry
Dim Shared As ULongInt ticks, totalTicks
Dim Shared As UInteger status_timer, vblanks, mousex,mousey, mousebuttons
Dim shared As Integer mousewheel
Dim Shared As Any Ptr framebuffer
Dim Shared As Integer PPUbuffer(256,240), backbuffer(256,240), oldbuffer(256,240)
Dim Shared As uinteger masterPalette(64) = {&h545454, &h001E74, &h081090, &h300088, &h440064, &h5C0030, &h540400, &h3C1800, &h202A00, &h083A00, &h004000, &h003C00, &h00323C, &h000000, &h000000, &h000000, &h989698, &h084CC4, &h3032EC, &h5C1EE4, &h8814B0, &hA01464, &h982220, &h783C00, &h545A00, &h287200, &h087C00, &h007628, &h006678, &h000000, &h000000, &h000000, &hECEEEC, &h4C9AEC, &h787CEC, &hB062EC, &hE454EC, &hEC58B4, &hEC6A64, &hD48820, &hA0AA00, &h74C400, &h4CD020, &h38CC6C, &h38B4CC, &h3C3C3C, &h000000, &h000000, &hECEEEC, &hA8CCEC, &hBCBCEC, &hD4B2EC, &hECAEEC, &hECAED4, &hECB4B0, &hE4C490, &hCCD278, &hB4DE78, &hA8E290, &h98E2B4, &hA0D6E4, &hA0A2A0, &h000000, &h000000}
Dim Shared As UByte button_counter, button(0 To 7)
Dim Shared As UInteger res(10,10)
res(1,1) = 640
res(1,2) = 480
res(2,1) = 800
res(2,2) = 600
res(3,1) = 1024
res(3,2) = 768
res(4,1) = 1280
res(4,2) = 960



Type cpus
	'---------------------------'
	'   6502 Registers/MEM etc  '
	'---------------------------'
	#Ifdef debugmode
	'======================================================ONLY INCLUDED IF DEBUGMODE IS DEFINED!======================================================================
	oldpc As UShort 'save pc for debug
	oldsp As UByte 'save sp for debug
	oldacc As UByte
	oldx As UByte
	oldy As UByte
	oldps As UByte
	'==================================================================================================================================================================
	#EndIf
	acc As UByte 'accumulator
	X As UByte 'X register
	Y As UByte 'Y register
	PS As UByte 'Processor status register
	'======These macros are for manipulating the flags in the 6502 processor status register. They are NOT part of the UDT(i.e. don't reference them with cpu.=========
	#define  Flag_S         ( cpu.ps And 128 ) / 128 'Sign flag bit 7
	#define  Flag_V         ( cpu.ps And 64 ) / 64 'Overflow flag bit 6
	#define  Flag_U         ( cpu.ps And 32 ) / 32 'Useless flag bit 5
	#define  Flag_B         ( cpu.ps And 16 ) / 16 'Break flag bit 4
	#define  Flag_D         ( cpu.ps And 8 ) / 8 'Decimal flag bit 3
	#define  Flag_I         ( cpu.ps And 4 ) / 4 'Interrupt flag bit 2
	#define  Flag_Z         ( cpu.ps And 2 ) / 2 'Zero flag bit 1
	#define  Flag_C         ( cpu.ps And 1 ) 'Carry flag bit 0
	#Define set_S cpu.ps = cpu.ps Or 128 'set sign flag
	#Define set_V cpu.ps = cpu.ps Or 64  'set overflow flag
	#Define set_U cpu.ps = cpu.ps Or 32  'set useless flag
	#Define set_B cpu.ps = cpu.ps Or 16  'set break flag
	#Define set_D cpu.ps = cpu.ps Or 8   'set decimal flag
	#Define set_I cpu.ps = cpu.ps Or 4   'set interrupt flag
	#Define set_Z cpu.ps = cpu.ps Or 2   'set zero flag
	#Define set_C cpu.ps = cpu.ps Or 1   'set carry flag
	#Define clear_S cpu.ps = cpu.ps And 127 'clear sign flag
	#Define clear_V cpu.ps = cpu.ps And 191 'clear overflow flag
	#Define clear_U cpu.ps = cpu.ps And 223 'clear useless flag
	#Define clear_B cpu.ps = cpu.ps And 239 'clear break flag
	#Define clear_D cpu.ps = cpu.ps And 247 'clear decimal flag
	#Define clear_I cpu.ps = cpu.ps And 251 'clear interrupt flag
	#Define clear_Z cpu.ps = cpu.ps And 253 'clear zero flag
	#Define clear_C cpu.ps = cpu.ps And 254 'clear carry flag
	PC As UShort 'program counter
	sp As UByte = &hFD 'stack pointer
	memory(0 To 65535) As UByte 'RAM
End Type

Type ppus
	'----------------------------'
	'   2C02 Registers/MEM etc   '
	'----------------------------'
	sprRAM (0 To &hFF) As UInteger
	tempSPRram (0 To 7, 0 To 3) As uinteger
	vram(0 To &hFFFF) As ubyte
	scanline As UInteger = 241
	sprAddr As UShort
	vrAddr As UInteger
	addrLatch As UByte
	basePatternAddress As UInteger
	curTile As UInteger
	tableLine As UInteger
	attrbLine As UInteger
	curAttrb As UInteger
	paletteIndex As UInteger
	finalPalette As UInteger
	curPixel As UByte
	sprCount As UByte
	sprHeight As UByte
	sprAddress As UInteger
	sprTileNumber As UInteger
	finex As uinteger
	ubit As UByte
	lbit As Ubyte
End Type

Type headers
	'------------------------'
	'      iNES Header       '
	'------------------------'
	signature(0 To 3) As Byte
	prgSize As UByte 'in 16KB pages
	chrSize As UByte 'in 8KB pages
	Flags6 As UByte
	Flags7 As UByte
	prgRAMSize As UByte 'in 8kb pages
	Flags9 As UByte
	Flags10 As UByte
	zeros (0 To 4) As UByte
End Type

ReDim Shared As UByte rom(0 To 1) 'Here we are preparing these dynamic arrays which will later be resized to the exact size they need
ReDim Shared As UByte prgROM(0 To 1)
ReDim Shared As UByte chrROM(0 To 1)
ReDim Shared As UByte prgRAM(0 To 1)

#Ifdef debugmode
'======================================================ONLY INCLUDED IF DEBUGMODE IS DEFINED!======================================================================
Dim Shared As cpus nint
'==================================================================================================================================================================
#EndIf

Dim Shared cpu As cpus '6502 CPU
Dim Shared ppu As ppus 'slightly less suspicious PPU (but still pretty suspicious)
Dim Shared header As headers 'iNES header
'================================================================PPU related macros================================================================================
#define PPUCTRL cpu.memory(&h2000)
#Define PPUMASK cpu.memory(&h2001)
#Define PPUSTATUS cpu.memory(&h2002)
#Define OAMADDR cpu.memory(&h2003)
#Define OAMDATA cpu.memory(&h2004)
#Define PPUSCROLL cpu.memory(&h2005)
#Define PPUADDR cpu.memory(&h2006)
#Define PPUDATA cpu.memory(&h2007)
#Define OAMDMA cpu.memory(&h4014)
#Define vblank            (ppustatus And 128) / 128
#define  PPUCTRL_V        ( ppuctrl And 128 ) / 128
#Define  PPUCTRL_P        ( ppuctrl And 64 ) / 64
#Define  PPUCTRL_H        ( ppuctrl And 32 ) / 32
#Define  PPUCTRL_B        (( ppuctrl And 16 ) / 16) * &h1000
#Define  PPUCTRL_S        (( PPUCTRL And 8 ) / 8) * &h1000
#Define  PPUCTRL_I        (( PPUCTRL And 4 ) / 4)
#Define  PPUCTRL_NN      (( PPUCTRL And 3 ) * &h400 ) + &h2000
#Define PPUMASK_INTENSIFY_B    ( PPUMASK And 128 ) / 128
#Define PPUMASK_INTENSIFY_G    ( PPUMASK And 64 ) / 64
#Define PPUMASK_INTENSIFY_R    ( PPUMASK And 32 ) / 32
#Define  PPUMASK_S       ( PPUMASK And 16 ) / 16
#Define  PPUMASK_B       ( PPUMASK And 8 ) / 8
#Define  PPUMASK_SL      ( PPUMASK And 4 ) / 4
#Define  PPUMASK_BL      ( PPUMASK And 2 ) / 2
#Define  PPUMASK_G       ( PPUMASK And 1 )
#Define  PPUSTATUS_V     ( PPUSTATUS And 128 ) / 128
#Define  PPUSTATUS_S     ( PPUSTATUS And 64 ) / 64
#Define  PPUSTATUS_O     ( PPUSTATUS And 32 ) / 32
#Define spr16Address (ppu.tempSPRram(spr,1) And 1) * &h1000
#Define sprPriority (ppu.tempSPRram(spr,2) And 32) / 32
#Define sprPalAddress (ppu.tempSPRram(spr,2) And 3)
#Define flipY (ppu.tempSPRram(spr,2) And 128) / 128
#Define flipX (ppu.tempSPRram(spr,2) And 64) / 64
'==================================================================================================================================================================
#Include Once "inc/loadrom.bi"
loadini
ChDir ExePath
ChDir("..")
#Include Once "inc/mapper.bi" 'mapper loading and bank switching
#Include Once "inc/misc.bi" 'misc stuff
#Include Once "inc/Controller.bi" 'Pad related functions and stuff
#Include Once "inc/ppu.bi" 'PPU related functions and stuff
#Include Once "inc/6502_instruction_set.bi" ' contains the instruction set
#Include Once "inc/decoder.bi" ' decodes hex opcodes to asm
emulatorMode = "6502"
lastframetime = Timer
version = "0.80 alpha"
debug = 1
opstoskip = 1
nextskip = 1

/'=================================================================================================================================================================
                                                                            Subroutines
==================================================================================================================================================================='/

Sub status 'This sub prints the status of various things to the screen
	'======================================================ONLY INCLUDED IF DEBUGMODE IS DEFINED!======================================================================
	#Ifdef debugmode
	'Draw String framebuffer, (0,0), "CoarseX: " & cxscroll & " CoarseY: " & cyscroll & " FineX: " & fxscroll & " FineY: " & fyscroll
	Draw String framebuffer, (0,10), "Mouse - X:" & mousex & " Y:"& mousey & " Buttons:" & mousebuttons & " Wheel:" & mousewheel
	Draw String framebuffer, (0,20), "PRG size: " & header.prgSize*16 & " | " & header.prgSize*16*1024
	Draw String framebuffer, (0,30), "Mapper: " & mapper & " | Save State Slot: " & saveSlot
	Draw String framebuffer, (0,40), "Total ops: " & totalops & " | Stepping by: " & opstoskip
	Draw String framebuffer, (0,50), "Ops per second: " &  Format(opsPerSecond, "0") & " | CPU Frequency: " &  Format(ticksPerSecond/1000000,"0.000") & "Mhz"
	Draw String framebuffer, (0,60), "________________________"
	Draw String framebuffer, (0,70), "A: " & IIf(cpu.acc < &h10,"0" & Hex(cpu.acc),Hex(cpu.acc)) & " X: " & IIf(cpu.x < &h10,"0" & Hex(cpu.x),Hex(cpu.x)) & " Y: " & IIf(cpu.y < &h10,"0" & Hex(cpu.y),Hex(cpu.y))
	Draw String framebuffer, (0,80), "PC: " & cpu.PC & " ($" & Hex(cpu.pc) & ")"
	Draw String framebuffer, (0,90), "Stack pointer: " & cpu.sp & "($" & Hex(cpu.sp) & ")"
	Draw String framebuffer, (0,110), "N   V   -   B   D   I   Z   C"
	Draw String framebuffer, (0,120), Flag_S & "   " & Flag_V & "   " & flag_U & "   " & flag_B & "   " & flag_D & "   " & flag_I & "   " & flag_Z & "   " & flag_C
	Draw String framebuffer, (0,140), "Processor status: " & cpu.ps & " " & " (" & Hex(cpu.ps) & ")"
	Draw String framebuffer, (0,150), "Message: " & msg
	'msg = ""
	Draw String framebuffer, (0,160), "Trace: "
	Line framebuffer, (0,100)-(240,130),RGB(255,255,255),b
	For q As UByte = 1 To 60
		Draw String framebuffer, (0,160 + (q*10)), opHistory(q)
	Next
	#EndIf
	'==================================================================================================================================================================
	Put framebuffer, (screenx-300,6),strawberry, Alpha
	Draw String framebuffer, (screenx - 250,60), "Version " & version
	Draw String framebuffer, (screenx - 270, 70), "Blyss Sarania & Nobbs66"
	Draw String framebuffer, (screenx - 210,80), Format(fps,"0.00") & " fps"
End Sub

Sub clear_framebuffer 'Just clears the main framebuffer
	For q As UInteger = 0 To screeny
		Line framebuffer, (0,q)-(screenx,q), 0
	Next
	For yyy As UInteger = 0 To 239
		For xxx As UInteger = 0 To 255
			oldbuffer(xxx,yyy) = ppubuffer(xxx,yyy)
			ppubuffer(xxx,yyy) = -1
		Next
	Next
End Sub

Sub push_framebuffer 'Outputs the main framebuffer to the screen
	If fitwindow = 0 Then status
	If fitwindow = 1 Then
		Draw String framebuffer, (screenx - 80,screeny - 10), Format(fps,"0.00") & " fps"
	EndIf
	Put(0,0),framebuffer,PSet
	clear_framebuffer
End Sub

Sub initcpu 'initialize CPU and RAM
	For i As Integer = 0 To 65535
		cpu.memory(i) = 0
		ppu.vram(i) = 0
	Next
	clear_framebuffer
	clear_s
	clear_z
	set_i
	Clear_d
	clear_c
	clear_v
	clear_b
	set_u
	cpu.PS = &h24 'init status
	ppuctrl = 0 '00000000
	ppumask = 0 '00000000
	ppustatus = 160 '10100000
	oamaddr = 0 '00000000
	ppuscroll = 0 '00000000
	ppuaddr = 0 '00000000
	ppudata = 0 '00000000
End Sub

Sub nmi 'Non maskable interrupt
	writemem((cpu.sp+&h100),(cpu.pc Shr 8))
	cpu.sp -=1
	writemem((cpu.sp+&h100), (cpu.pc And &hff))
	cpu.sp -=1
	writemem(cpu.sp+&h100,cpu.ps)
	cpu.sp -=1
	Dim suspicious_pointer As UShort Ptr
	Dim suspicious_array(0 To 1) As UByte
	suspicious_array(0) = readmem(&HFFFA)
	suspicious_array(1) = readmem(&HFFFB)
	suspicious_pointer = @suspicious_array(0)
	cpu.pc = *suspicious_pointer
	ticks+=7
	'For i As Integer = 255 To 1 Step -1
	'	opHistory(i) = opHistory(i-1)
	'Next
	'opHistory(0) = "NMI fired on " & totalops & " ops."
End Sub

Function readmem(ByVal addr As ULongInt, ByVal numbytes As UInteger = 1) As UShort
	If addr >= &h800 and addr < &h2000 then addr And= &h800
	Select Case addr
		Case &h2000 To &h3FFF
			readmem = readPPUreg(addr and &h2007)
		Case &h4015
			'apu stuff
		Case &h4016
			'controller stuff
		Case Else
			Dim As UShort Ptr suspicious_pointer
			Dim As UByte tempmem(0 To 1)
			For q As UByte = 0 To numbytes-1
				tempmem(q)=cpu.memory(addr+q)
			Next
			If addr = &hff Andalso numbytes = 2 Then tempmem(1) = cpu.memory(0):ticks+=1:End if
			suspicious_pointer=@tempmem(0)
			readmem = *suspicious_pointer
	End Select
End Function

Sub writemem(ByVal addr As ULongInt, ByVal value As UByte) 'write memory
	If emulatorMode = "6502" Then
		cpu.memory(addr) = value
	Else
		if addr >= &h800 and addr < &h2000 then addr And= &h800
		Select Case addr
			Case &h2000 To &h3FFF
				writePPUreg(addr and &h2007, value)
			Case &h4014
				writePPUreg(&H4014,value)
			Case &h4000 To &h4015, &h4017
				'apu stuff
			Case &h4016
				PadWrite
			Case &h8000 To &hffff
				'Swap banks
				bankSwap(value,addr)
			Case Else
				cpu.memory(addr) = value
		End Select
	End If
End Sub

Sub loadini 'load the ini. Duh.
	Dim f As UByte = FreeFile
	If Not FileExists(ExePath & "\strawberry.ini") Then
		Open ExePath & "\strawberry.ini" For Output As #f
		Print #f, 640
		Print #f, 480
		Print #f, 10000
		Print #f, 1
		Print #f, 1
		Print #f, 0
		Print #f, 0
		Print #f, ""
		Close #f
	EndIf
	Open ExePath & "\strawberry.ini" For Input As #f
	Input #f, screenx
	Input #f, screeny
	Input #f, opgoal
	Input #f, flimit
	Input #f, sf
	Input #f, do_trace
	Input #f, lastrom
	Input #f, fitwindow
	Close #f
	centerx = screenx/2
	centery = screeny/2
End Sub

Sub writeini 'write out a new ini
	Dim f As UByte = FreeFile
	Open ExePath & "\strawberry.ini" For output As #f
	Print #f, screenx
	Print #f, screeny
	Print #f, opgoal
	Print #f, fLimit
	Print #f, sf
	Print #f, do_trace
	Print #f, lastrom
	Print #f, fitwindow
	Close #f
End Sub
Sub CAE
	If strawberry Then ImageDestroy(Strawberry)
	Close
	End
End Sub

sub frameLimit
	fps = vblanks/(Timer - vStart)
	'Limit FPS
	If flimit = 1 Then
		While  ticksPerSecond > 1789000  OrElse fps > 60 '1.789Mhz or 60FPS
			ticksPerSecond = TotalTicks/(Timer - start)
			fps = vblanks/(Timer - vStart)
		Wend
	End if
	'This gives the FPS timer a "resolution" so to speak.
	'Every 1/4 of a second the timer is restarted and the old amount
	'Is weighted and carried forward, this is so the game does not
	'lag when switching from say a menu with low requirements to
	'the main game.
	If Timer - vStart >= 1 Then
		vblanks = fps/2
		vStart = Timer - .5
	EndIf
End Sub

Sub options
	While MultiKey(SC_O):Sleep 1,1:wend
	Dim As UInteger optXsize=500, optYsize=300
	Dim As UInteger halfX = optXsize / 2, halfY = optYsize/2
	Dim As uinteger tempcolor = RGB(255,255,255)
	Dim As Integer oldwheel
	Dim As Byte resindex =-1, firstloop = 1, oldfitwindow = fitwindow, oldsf = sf
	For qq As Integer = 0 To 9
		If screenx = res(qq,1) Then resindex = qq
	Next
	If resindex = -1 Then resindex = 1
	Do
		oldwheel = mousewheel
		GetMouse(mousex,mousey,mousewheel,mousebuttons)
		If firstloop = 1 Then
			oldwheel = mousewheel
			firstloop = 0
		EndIf
		Put(0,0),framebuffer,PSet
		clear_framebuffer
		ppuRender
		Line framebuffer, (centerx - halfx, centery-halfY)-(centerx + halfx, centery+halfy), RGB(0,50,150), BF
		Line framebuffer, (centerx - halfx, centery-halfY)-(centerx + halfx, centery-(halfy-10)), RGB(0,30,80), BF
		Line framebuffer, (centerx - halfx, centery-(halfy-10))-(centerx + halfx, centery-(halfy-10)), RGB(255,255,255)
		Line framebuffer, (centerx - halfx, centery-halfY)-(centerx + halfx, centery+halfy), RGB(255,255,255),B
		draw String framebuffer, (centerx-28,centery-(halfy-1)), "Options"
		If mousex > (centerx - halfx) And mousex < ((centerx-halfx)+184) Then
			If mousey > (centery - halfy +(10)) And mousey < (centery - halfy +(25)) Then
				tempcolor = RGB(150,0,255)
				If mousewheel <> oldwheel Then
					flimit+= (mousewheel-oldwheel)
					If flimit = 2 Then flimit = 0
					If flimit > 2 Then flimit = 1
					totalops = opgoal * (Timer-start) 'force totalops to be "where it should" since we are changing the goal
				EndIf
			EndIf
		EndIf
		Draw String framebuffer, (centerx-halfX,(centery-halfy)+15), "Enable frame limit: " & IIf(flimit = 1, "Yes", "No"), tempcolor
		tempcolor = RGB(255,255,255)

		If mousex > (centerx - halfx) And mousex < ((centerx-halfx)+208) Then
			If mousey > (centery - halfy +(24)) And mousey < (centery - halfy +(39)) Then
				tempcolor = RGB(150,0,255)
				If mousewheel <> oldwheel Then
					sf+= (mousewheel-oldwheel)
					if sf > 6 then sf = 1
					If SF = 0 Then SF = 6
					For q As UInteger = 0 To screeny
						Line nesbuffer, (0,q)-(screenx,q), 0'Blank the nes buffer since the scaling size has changed
					Next
					forcerender = 1 'force next frame to render ALL pixels since we blanked the framebuffer
				EndIf
			EndIf
		EndIf
		Draw String framebuffer, (centerx-halfx,(centery-halfy)+30), "NES image scale factor: " & sf & "x", tempcolor
		tempcolor = RGB(255,255,255)

		If mousex > (centerx - halfx) And mousex < ((centerx-halfx)+370) Then
			If mousey > (centery - halfy +(38)) And mousey < (centery - halfy +(53)) Then
				tempcolor = RGB(150,0,255)
				If mousewheel <> oldwheel Then
					opgoal += (mousewheel -oldwheel) * 500
					totalops = opgoal * (Timer-start) 'force totalops to be "where it should" since we are changing the goal
				EndIf
			EndIf
		EndIf
		Draw String framebuffer, (centerx-halfx,(centery-halfy)+45), "Op/second limit for simple 6502 programs: " & opgoal, tempcolor
		tempcolor = RGB(255,255,255)

		If mousex > (centerx - halfx) And mousex < ((centerx-halfx)+182) Then
			If mousey > (centery - halfy +(52)) And mousey < (centery - halfy +(67)) Then
				tempcolor = RGB(150,0,255)
				If mousewheel <> oldwheel Then
					resindex += (mousewheel-oldwheel)
					If resindex = 0 Then resindex = 1
					If res(resindex,1) = 0 Then resindex-=1
				EndIf
			EndIf
		EndIf
		Draw String framebuffer, (centerx-halfx,(centery-halfy)+60), "Window size: " & res(resindex,1) & "x" & res(resindex,2), tempcolor
		tempcolor = RGB(255,255,255)

		If mousex > (centerx - halfx) And mousex < ((centerx-halfx)+248) Then
			If mousey > (centery - halfy +(66)) And mousey < (centery - halfy +(82)) Then
				tempcolor = RGB(150,0,255)
				If mousewheel <> oldwheel Then
					do_trace+= (mousewheel-oldwheel)
					If do_trace = 2 Then do_trace = 0
					If do_trace > 2 Then do_trace = 1
					totalops = opgoal * (Timer-start) 'force totalops to be "where it should" since we are changing the goal
				EndIf
			EndIf
		EndIf
		Draw String framebuffer, (centerx-halfx,(centery-halfy)+75), "Enable opcode trace(slow!): " & IIf(do_trace = 1, "Yes", "No"), tempcolor
		tempcolor = RGB(255,255,255)

		If mousex > (centerx - halfx) And mousex < ((centerx-halfx)+453) Then
			If mousey > (centery - halfy +(83)) And mousey < (centery - halfy +(97)) Then
				tempcolor = RGB(150,0,255)
				If mousewheel <> oldwheel Then
					fitwindow+= (mousewheel-oldwheel)
					If fitwindow = 2 Then fitwindow = 0
					If fitwindow > 2 Then fitwindow = 1
					totalops = opgoal * (Timer-start) 'force totalops to be "where it should" since we are changing the goal
				EndIf
			EndIf
		EndIf
		Draw String framebuffer, (centerx-halfx,(centery-halfy)+90), "Fit scaled NES image to window(no debug info at all): " & IIf(fitwindow = 1, "Yes", "No"), tempcolor
		tempcolor = RGB(255,255,255)

		If fitwindow = 0 Then status
		If mousex < screenx And mousey < screeny Then
			While mousebuttons <> 0
				GetMouse(mousex,mousey,,mousebuttons)
				Sleep 1,1
			Wend
		End If
		Draw String framebuffer, (centerx-halfx,centery+halfy-25), "Use mousewheel to change highlighted values!", RGB(255,255,255)
		Draw String framebuffer, (centerx-halfx,centery+halfy-10), "Press O to return to emulation!", RGB(255,255,255)

	Loop While Not MultiKey(SC_O) And Not MultiKey(SC_ESCAPE)
	If MultiKey(SC_ESCAPE) Then CAE
	If (res(resindex,1) <> screenx) OrElse (oldfitwindow <> fitwindow) OrElse (oldsf <> sf) Then
		If fitwindow = 1 Then
			screenx = 256*sf
			screeny = 240*sf
		Else
			screenx = res(resindex,1)
			screeny = res(resindex,2)
		EndIf
		centerx = screenx/2
		centery = screeny/2
		While (256*sf > screenx) OrElse (240*sf > screeny)
			sf-=1
		Wend
		ImageDestroy(framebuffer)
		ImageDestroy(nesbuffer)
		framebuffer = ImageCreate(screenx,screeny,RGB(0,0,0))
		nesbuffer = ImageCreate(screenx,screeny,RGB(255,0,255))
		ScreenRes(screenx,screeny,32)
		While sf*256 > ScreenX or sf*240 > screeny
			sf-=1
			If sf = 1 Then Exit while
		Wend
		For q As UInteger = 0 To screeny
			Line nesbuffer, (0,q)-(screenx,q), 0'Blank the nes buffer since the scaling size has changed
		Next
		forcerender = 1 'force next frame to render ALL pixels since we blanked the framebuffer
	End If

	If do_trace = 0 Then
		For qq As Uinteger = 0 To 255
			ophistory(qq) = ""
		Next
	EndIf
	writeini
End Sub

Sub romInfo
	While MultiKey(SC_F11):Sleep 1,1:wend
	Dim As UInteger optXsize=300, optYsize=60
	Dim As UInteger halfX = optXsize / 2, halfY = optYsize/2
	Dim As uinteger tempcolor = RGB(255,255,255)
	Dim As Integer oldwheel
	Dim As Byte resindex =-1, firstloop = 1, oldfitwindow = fitwindow, oldsf = sf
	dim as string mappertext(0 to 10) = {"0 - NROM", "1 - MMC1", "2 - UNROM", "3 - CNROM", "4 - MMC3", "5 - MMC5", "6 - Mapper 6", "7 - AOROM", "8 - Mapper 8", "9 - MMC2"}

	For qq As Integer = 0 To 9
		If screenx = res(qq,1) Then resindex = qq
	Next
	If resindex = -1 Then resindex = 1
	Do
		oldwheel = mousewheel
		GetMouse(mousex,mousey,mousewheel,mousebuttons)
		If firstloop = 1 Then
			oldwheel = mousewheel
			firstloop = 0
		EndIf
		Put(0,0),framebuffer,PSet
		clear_framebuffer
		ppuRender
		Line framebuffer, (centerx - halfx, centery-halfY)-(centerx + halfx, centery+halfy), RGB(0,50,150), BF
		Line framebuffer, (centerx - halfx, centery-halfY)-(centerx + halfx, centery-(halfy-10)), RGB(0,30,80), BF
		Line framebuffer, (centerx - halfx, centery-(halfy-10))-(centerx + halfx, centery-(halfy-10)), RGB(255,255,255)
		Line framebuffer, (centerx - halfx, centery-halfY)-(centerx + halfx, centery+halfy), RGB(255,255,255),B
		draw String framebuffer, (centerx-32,centery-(halfy-1)), "Rom Info"
		Draw String framebuffer, (centerx-halfx,centery-(halfy-12)), "File name: " & shpname
		Draw String framebuffer, (centerx-halfx,centery-(halfy-22)), "Region: " & TVsystem
		Draw String framebuffer, (centerx-halfx,centery-(halfy-32)), "PRG Size: " & header.prgSize*16 & "KB | CHR Size: " & header.chrSize*8 & "KB"
		Draw String framebuffer, (centerx-halfx,centery-(halfy-42)), "Mapper: " & mappertext(mapper)
		Draw String framebuffer, (centerx-halfx,centery-(halfy-52)), "Mirroring: " & mirroring


	Loop While Not MultiKey(SC_ESCAPE) AndAlso Not MultiKey(SC_f11)
	While MultiKey(SC_F11):Sleep 1,1: wend

End Sub


Sub savestate
	Dim As UInteger savex, savey
	Dim As UByte f = FreeFile

	Open ExePath & "/states/" & gamename & "_strawberry.state" & saveSlot For Binary As #f
	Put #f, , cpu
	put #f, , ppu
	put #f, , header
	put #f, , prgROM()
	put #f, , chrROM()
	put #f, , prgRAM()
	put #f, , PPUbuffer()
	put #f, , backbuffer()
	put #f, , oldbuffer()
	close #f

	Open ExePath & "/states/" & gamename & "_strawberry.vars" & saveSlot For Binary As #f
	Print #1, debug
	Print #1, mapper
	Print #1, spritehit
	Print #1, flimit
	Print #1, sf
	Print #1, forcerender
	Print #1, backIsTransparent
	Print #1, opstoskip
	Print #1, nextskip
	Print #1, romsize
	Print #1, totalops
	Print #1, ticksPerSecond
	Print #1, emulatorMode
	Print #1, instruction
	Print #1, amode
	Print #1, shpname
	Print #1, mirroring
	Print #1, TVsystem
	Print #1, start
	Print #1, lastframetime
	Print #1, opsPerSecond
	Print #1, stepstart
	Print #1, fps
	Print #1, vstart
	Print #1, curtime
	Print #1, ticks
	Print #1, totalTicks
	Print #1, status_timer
	Print #1, vblanks
	For savey As UInteger = 0 To screeny
		For savex As UInteger = 0 To screenx
			Print #1, Point(savex,savey)
		Next
	Next
	Close #f
	msg = "Save state in slot " & saveSlot & " was saved successfully!"
End Sub

Sub loadstate
	Dim As UInteger savex, savey, savecolor
	Dim As UByte f = FreeFile, errd
	If Not fileexists (ExePath & "/states/" & gamename & "_strawberry.state" & saveSlot) Then errd = 2
	If Not fileexists (ExePath & "/states/" & gamename & "_strawberry.vars" & saveSlot) Then errd = 2
	If errd = 2 Then
		msg = "Save state does not exist or is corrupt!"
		Exit sub
	EndIf
	Open ExePath & "/states/" & gamename & "_strawberry.state" & saveSlot For Binary As #f
	Get #f, , cpu
	Get #f, , ppu
	Get #f, , header
	ReDim As UByte PrgROM(header.prgSize*16*1024)
	ReDim As UByte chrROM(header.chrSize*8*1024)
	ReDim As UByte prgRAM(header.prgRAMSize*8*1024)
	Get #f, , prgROM()
	Get #f, , chrROM()
	get #f, , prgRAM()
	Get #f, , PPUbuffer()
	Get #f, , backbuffer()
	get #f, , oldbuffer()
	close #f

	Open ExePath & "/states/" & gamename & "_strawberry.vars" & saveSlot For Binary As #f
	Input #1, debug
	input #1, mapper
	Input #1, spritehit
	Input #1, flimit
	Input #1, sf
	Input #1, forcerender
	Input #1, backIsTransparent
	Input #1, opstoskip
	Input #1, nextskip
	Input #1, romsize
	Input #1, totalops
	Input #1, ticksPerSecond
	Input #1, emulatorMode
	Input #1, instruction
	Input #1, amode
	Input #1, shpname
	Input #1, mirroring
	Input #1, TVsystem
	Input #1, start
	Input #1, lastframetime
	Input #1, opsPerSecond
	Input #1, stepstart
	Input #1, fps
	Input #1, vstart
	Input #1, curtime
	Input #1, ticks
	Input #1, totalTicks
	Input #1, status_timer
	Input #1, vblanks
	For savey As UInteger = 0 To screeny
		For savex As UInteger = 0 To screenx
			Input #1, savecolor
			PSet nesbuffer, (savex,savey), savecolor
		Next
	Next
	Close #f
	msg = "Save state in slot " & saveslot & " was loaded successfully!"
End Sub
'======================================================ONLY INCLUDED IF DEBUGMODE IS DEFINED!======================================================================
#Ifdef debugmode
Sub comparelog
	nint.pc = 0
	opdata = ""
	opc = ""
	nint.acc = 0
	nint.x = 0
	nint.y = 0
	nint.ps = 0
	nint.sp = 0
	nintcyc = 9
	nintsl = 0
	Line Input #31, curline
	nint.pc = CuInt("&h" & Left(curline,4))
	opdata = Right(Left(curline,16),12) 'deal with
	opc = Right(Left(curline,19),3)
	nint.acc = CUInt("&h" & Right(Left(curline,52),2))
	nint.x = CUInt("&h" & Right(Left(curline,57),2))
	nint.y = CUInt("&h" & Right(Left(curline,62),2))
	nint.ps = CUInt("&h" & Right(Left(curline,67),2))
	nint.sp = CUInt("&h" & Right(Left(curline,73),2))
	nintcyc = CUInt(Right(Left(curline,81),3))
	nintsl = CUInt(Right(curline,3))
	If nint.pc <> cpu.oldpc Then fail("CPU.PC", Str(Hex(nint.pc)), Str(Hex(cpu.oldpc)))
	If opc <> instruction Then fail("Decoder", opc, instruction)
	If nint.acc <> cpu.oldacc Then fail("CPU.ACC", Str(Hex(nint.acc)), Str(Hex(cpu.oldacc)))
	If nint.x <> cpu.oldx Then fail("CPU.X", Str(Hex(nint.x)), Str(Hex(cpu.oldx)))
	If nint.y <> cpu.oldy Then fail("CPU.Y", Str(Hex(nint.y)), Str(Hex(cpu.oldy)))
	If nint.ps <> cpu.oldps Then fail("CPU.PS", Str(Hex(nint.ps)), Str(Hex(cpu.oldps)))
	If nint.sp <> cpu.oldsp Then fail("CPU.sp", Str(Hex(nint.sp)), Str(Hex(cpu.oldsp)))
	'If nintsl <> ppu.scanline Then fail("scanline", Str(nintsl), Str(ppu.scanline))
End Sub

Sub fail (ByVal op As String, ByVal expected As String, ByVal actual As String)
	Cls
	Print "Log comparison failed on op " & totalops
	Print op & " was expected to be " & expected & " but was actually " & actual
	Print
	Print "Full line of the log in question: "
	Print curline
	Print
	Print "Taddr: " & taddr
	Print "*Tdata: " & *tdata
	Print Hex(cpu.acc)
	Print amode
	Print cpu.memory(&h2ff)
	Print "Paused. Press Space to resume or Escape to exit!"
	Do
		Sleep 10
		If MultiKey(SC_ESCAPE) Then CAE
	Loop While Not MultiKey(SC_SPACE)
	While MultiKey(SC_SPACE):Sleep 10,1: wend
End Sub

Sub write_the_log
	Print #99, "Op number: " & totalops
	Print #99, ophistory(0)
	Print #99, "CPU.PC: " & cpu.pc & " (0x" & Hex(cpu.pc) & ") | " & "CPU.SP: " & cpu.sp & " (0x" & Hex(cpu.sp) & ") | " & "CPU.PS: " & cpu.ps & " (0x" & Hex(cpu.ps) & ")"
	Print #99, "CPU.ACC: " & cpu.acc & " (0x" & Hex(cpu.acc) & ") | " & "CPU.X: " & cpu.x & " (0x" & Hex(cpu.x) & ") | " & "CPU.Y: " & cpu.y & " (0x" & Hex(cpu.y) & ")"
	Print #99, "-----------------"
	Print #99, "|N|V|-|B|D|I|Z|C|"
	Print #99, "|" & flag_S & "|" & flag_v & "|" & flag_u & "|" & flag_b & "|" & flag_d & "|" & flag_i & "|" & flag_z & "|" & flag_c & "|"
	Print #99, "-----------------"
	Print #99, "----------------------------------------------------------------------------------------------------------------"
End Sub
#EndIf
'==================================================================================================================================================================

/'=================================================================================================================================================================
                                                                End subroutines
==================================================================================================================================================================='/

/'=================================================================================================================================================================
                                                                 Main program
==================================================================================================================================================================='/
ScreenRes screenx,screeny,32
framebuffer = ImageCreate(screenx,screeny,RGB(0,0,0))
nesbuffer = ImageCreate(screenx,screeny,RGB(255,0,255))
strawberry = freeimage_load_fb(CurDir & "/Res/SBNES.png", TRUE) ' load cute strawberry :)
initcpu 'initialize the 6502 and 2C02
loadROM 'load file into ROM and cpu memory
Cls
If emulatorMode = "6502" Then cpu.pc = &h0600 'set pc to program start for simple 6502 stuff
start = Timer

#Ifdef debugmode
If logops = 1 Then Open "log.txt" For Output As #99
#EndIf
vstart = Timer
Do
	'======================================================ONLY INCLUDED IF DEBUGMODE IS DEFINED!======================================================================
	#Ifdef debugmode
	cpu.oldps = cpu.ps
	cpu.oldsp = cpu.sp
	cpu.oldacc = cpu.acc
	cpu.oldx = cpu.X
	cpu.oldy = cpu.y
	cpu.oldpc = cpu.pc 'this is for storing debug information
	curtime = (Timer-start)
	opsPerSecond = totalops/curtime
	ticksPerSecond = TotalTicks/curtime
	#EndIf
	'==================================================================================================================================================================
	'====================================REMOVE THIS======================================================
	If emulatormode = "6502" Then
		cpu.memory(&hfe) = Rnd*255 ' random number generator for simple 6502 programs
		keycheck
	EndIf
	'====================================REMOVE THIS======================================================
	cpu.pc+=1
	totalops+=1
	decode_and_execute(cpu.memory(cpu.pc-1)) ' decode the binary located at the PC to opcode and address mode and then execute the instruction
	clear_b '==========================================================HACKS==========================================================================================
	If ticks >=111 And emulatorMode <> "6502" Then
		keycheck
		ppuLoop
		frameLimit
		totalTicks+=ticks
		ticks = ticks - 111
	EndIf
	#Ifdef debugmode
	If debug = 1 And nextskip = 0 Then
		stepstart = Timer
		nextskip = opstoskip
		While Not MultiKey(SC_SPACE) And Not MultiKey(SC_ESCAPE) And Not MultiKey(SC_PAGEUP)
			Sleep 100
			If MultiKey(SC_BACKSPACE) Then
				If opstoskip = 1 Then
					opstoskip = 10
				ElseIf opstoskip = 10 Then
					opstoskip = 100
				ElseIf opstoskip = 100 Then
					opstoskip = 1000
				ElseIf opstoskip = 1000 Then
					opstoskip = 1
				EndIf
				nextskip = opstoskip
				status
				While MultiKey(SC_BACKSPACE):Sleep 10: Wend
			EndIf
		Wend
		While MultiKey(SC_SPACE): Sleep 10: Wend
		start += (Timer-stepstart)
		If start > Timer Then start = Timer
		push_framebuffer
	End If
	status_timer+=1
	If status_timer >= OpsPerSecond/20 And emulatorMode = "6502" Then
		simplegraphics
		push_framebuffer
		status_timer = 0
	End If
	If logops = 1 Then write_the_log
	#EndIf
	'==================================================================================================================================================================
	If opsPerSecond > opgoal And emulatorMode = "6502" And flimit = 1 Then Sleep 10
Loop While Not MultiKey(SC_ESCAPE)
Close
CAE


