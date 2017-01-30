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
Randomize Timer
#Include Once "fbgfx.bi"
Using fb
#Include Once "string.bi"
#Include Once "crt.bi" 'c runtime
#Include Once "file.bi" 'File functions
#Include Once "Freeimage.bi" ' Freeimage library
#Include Once "inc/freetofb.bi" 'Easily use Freeimage images in Freebasic
#Include Once "zlib.bi"
Declare Function readmem(ByVal addr As ULongInt, ByVal numbytes As UInteger = 1) As Ushort ' for reading memory
Declare Sub writemem(ByVal addr As ULongInt, ByVal value As UByte) 'write a value to NES memory
Declare Sub status 'print various status stuff to the screen
Declare Sub initcpu 'initialize the 6502
Declare Sub loadROM 'load a ROM in to memory
Declare Sub CAE 'Cleanup and exit
Declare Sub loadini 'Load the ini file
Declare Sub nmi
Declare Sub write_the_log
Declare Sub push_Framebuffer
Declare Sub clear_framebuffer
Declare Sub comparelog
Declare Sub fail(ByVal op As String, ByVal expected As String, ByVal actual As String)
Dim Shared As UByte debug, trace_done = 0
Dim Shared As UInteger opstoskip, nextskip, opGoal, ticks, romsize, screenx, screeny, starts, totalops, logops=0
Dim Shared As String opHistory(0 To 255), emulatorMode, instruction, amode, msg, version
Dim Shared As Single start, lastframetime,opsPerSecond, stepstart
Dim Shared As Any Ptr strawberry
Dim Shared As UInteger status_timer, vblanks
Dim Shared As Any Ptr framebuffer
Dim Shared As uinteger masterPalette(64) = {&h545454, &h001E74, &h081090, &h300088, &h440064, &h5C0030, &h540400, &h3C1800, &h202A00, &h083A00, &h004000, &h003C00, &h00323C, &h000000, &h000000, &h000000, &h989698, &h084CC4, &h3032EC, &h5C1EE4, &h8814B0, &hA01464, &h982220, &h783C00, &h545A00, &h287200, &h087C00, &h007628, &h006678, &h000000, &h000000, &h000000, &hECEEEC, &h4C9AEC, &h787CEC, &hB062EC, &hE454EC, &hEC58B4, &hEC6A64, &hD48820, &hA0AA00, &h74C400, &h4CD020, &h38CC6C, &h38B4CC, &h3C3C3C, &h000000, &h000000, &hECEEEC, &hA8CCEC, &hBCBCEC, &hD4B2EC, &hECAEEC, &hECAED4, &hECB4B0, &hE4C490, &hCCD278, &hB4DE78, &hA8E290, &h98E2B4, &hA0D6E4, &hA0A2A0, &h000000, &h000000}
Dim Shared As UByte button_counter
Dim Shared As UByte button(0 To 7)
'======================================================THESE VARS ARE FOR THE LOG COMPARISON===================================================
Dim Shared As String curline
Dim Shared As String opdata, opc
Dim Shared As ULongInt nintcyc
Dim Shared As UInteger nintsl
Dim Shared As UByte logcomp = 1
'==============================================================================================================================================
Type cpus
	'------------------------'
	'   6502 Registers/MEM   '
	'------------------------'
	oldpc As UShort 'save pc for debug
	oldsp As UByte 'save sp for debug
	oldacc As UByte
	oldx As UByte
	oldy As UByte
	acc As UByte 'accumulator
	X As UByte 'X register
	Y As UByte 'Y register
	PS As UByte 'Processor status register
	oldps As UByte
	'bit 7 S Sign
	'bit 6 V Overflow
	'bit 5 unused(always 1)
	'bit 4 B Break
	'bit 3 D Decimal
	'bit 2 I Interrupt
	'bit 1 Z Zero
	'bit 0 C Carry
	#define  Flag_S         ( cpu.ps And 128 ) / 128 'Sign flag
	#define  Flag_V         ( cpu.ps And 64 ) / 64 'Overflow flag
	#define  Flag_U         ( cpu.ps And 32 ) / 32 'Useless flag
	#define  Flag_B         ( cpu.ps And 16 ) / 16 'Break flag
	#define  Flag_D         ( cpu.ps And 8 ) / 8 'Decimal flag
	#define  Flag_I         ( cpu.ps And 4 ) / 4 'Interrupt flag
	#define  Flag_Z         ( cpu.ps And 2 ) / 2 'Zero flag
	#define  Flag_C         ( cpu.ps And 1 ) 'Carry flag
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
	'nesReset As Integer 'reset vector
End Type

Type ppus
	sprRAM (0 To &hFF) As UByte
	tempSPRram (0 To 7, 0 To 3) As ubyte
	vram(0 To &hFFFF) As ubyte
	scanline As UInteger = 241
	sprAddr As UShort
	vrAddr As Uinteger
	addrLatch As UByte
	curTile As UInteger
	tableLine As UInteger
	attrbLine As UInteger
	curAttrb As UInteger
	Palette As UInteger
	ubit As UByte
	lbit As Ubyte
	curx As UInteger
	cury As uinteger
End Type

Type headers
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

ReDim Shared As UByte rom(0 To 1)
ReDim Shared As UByte prgROM(0 To 1)
ReDim Shared As UByte chrROM(0 To 1)
ReDim Shared As UByte prgRAM(0 To 1)
Dim Shared As cpus nint'==============================================================================================
Dim Shared cpu As cpus '6502 CPU
Dim Shared ppu As ppus 'slightly less suspicious PPU
Dim Shared header As headers
#define PPUCTRL cpu.memory(&h2000)
#Define PPUMASK cpu.memory(&h2001)
#Define PPUSTATUS cpu.memory(&h2002)
#Define OAMADDR cpu.memory(&h2003)
#Define OAMDATA cpu.memory(&h2004)
#Define PPUSCROLL cpu.memory(&h2005)
#Define PPUADDR cpu.memory(&h2006)
#Define PPUDATA cpu.memory(&h2007)
#Define OAMDMA cpu.memory(&h4014)
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
#Include Once "inc/loadrom.bi"
loadini ' need to load it here because of font stuff
ChDir ExePath
ChDir("..")
#Include Once "inc/misc.bi" 'misc stuff
#Include Once "inc/Controller.bi"
#Include Once "inc/ppu.bi" 'PPU
#Include Once "inc/6502_instruction_set.bi" ' contains the instruction set
#Include Once "inc/decoder.bi" ' decodes hex opcodes to asm
emulatorMode = "6502"
lastframetime = Timer
version = "0.60 alpha"
debug = 1
opstoskip = 1
nextskip = 1

/'==============================================================================
                                       Subroutines
================================================================================'/

Sub status
	Draw String framebuffer, (0,0), "Emulator mode: " & emulatorMode
	Draw String framebuffer, (0,10), "PRG size: " & header.prgSize*16 & " | " & header.prgSize*16*1024
	Draw String framebuffer, (0,20), "Total ops: " & totalops & " | Stepping by: " & opstoskip
	Draw String framebuffer, (0,30), "Ops per second: " &  opsPerSecond
	Draw String framebuffer, (0,40), "Registers: "
	Draw String framebuffer, (0,50), "________________________"
	Draw String framebuffer, (0,60), "A: " & IIf(cpu.acc < &h10,"0" & Hex(cpu.acc),Hex(cpu.acc)) & " X: " & IIf(cpu.x < &h10,"0" & Hex(cpu.x),Hex(cpu.x)) & " Y: " & IIf(cpu.y < &h10,"0" & Hex(cpu.y),Hex(cpu.y))
	Draw String framebuffer, (0,70), "PC: " & cpu.PC & " ($" & Hex(cpu.pc) & ")"
	Draw String framebuffer, (0,80), "Stack pointer: " & cpu.sp & "($" & Hex(cpu.sp) & ")"
	Draw String framebuffer, (0,100), "N   V   -   B   D   I   Z   C"
	Draw String framebuffer, (0,110), Flag_S & "   " & Flag_V & "   " & flag_U & "   " & flag_B & "   " & flag_D & "   " & flag_I & "   " & flag_Z & "   " & flag_C
	Draw String framebuffer, (0,130), "Processor status: " & cpu.ps & " " & " (" & Hex(cpu.ps) & ")"
	Draw String framebuffer, (0,140), "Message: " & msg
	msg = ""
	Draw String framebuffer, (0,150), "Trace: "
	Line framebuffer, (0,95)-(240,120),RGB(255,255,255),b
	For q As UByte = 1 To 60
		Draw String framebuffer, (0,150 + (q*10)), opHistory(q)
	Next
	Put framebuffer, (screenx-300,6),strawberry, Alpha
	Draw String framebuffer, (screenx - 250,60), "Version " & version
	Draw String framebuffer, (screenx - 270, 70), "Blyss Sarania & Nobbs66"
	Draw String framebuffer, (screenx - 210,80), Format(vblanks / (Timer-start),"0.00") & " fps"
End Sub

Sub clear_framebuffer
	For q As UInteger = 0 To screeny
		Line framebuffer, (0,q)-(screenx,q), 0
	Next
End Sub

Sub push_framebuffer
	status
	Put(0,0),framebuffer,PSet
	clear_framebuffer
End Sub
Sub initcpu 'initialize CPU and RAM
	For i As Integer = 0 To 65535
		cpu.memory(i) = 0
		ppu.vram(i) = 0
	Next

	clear_s
	clear_z
	set_i
	Clear_d
	clear_c
	clear_v
	'set_b
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

Sub nmi
	writemem((cpu.sp+&h100),(cpu.pc Shr 8))
	cpu.sp -=1
	writemem((cpu.sp+&h100), (cpu.pc And &hff))
	cpu.sp -=1
	writemem(cpu.sp+&h100,cpu.ps)
	cpu.sp -=1
	'set_i
	Dim suspicious_pointer As UShort Ptr
	Dim suspicious_array(0 To 1) As UByte
	suspicious_array(0) = readmem(&HFFFA)
	suspicious_array(1) = readmem(&HFFFB)
	suspicious_pointer = @suspicious_array(0)
	cpu.pc = *suspicious_pointer
	ticks+=7
	For i As Integer = 255 To 0 Step -1
		opHistory(i) = opHistory(i-1)
	Next
	opHistory(0) = "NMI fired on " & totalops & " ops."
End Sub

Function readmem(ByVal addr As ULongInt, ByVal numbytes As UInteger = 1) As UShort
	Select Case addr
		Case &h2000 To &h3FFF
			If addr = &h2002 Then cpu.memory(&h2002) And= 127
			readmem = readPPUreg(addr And &h2007)
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
			If addr = &hff Andalso numbytes = 2 Then tempmem(1) = cpu.memory(0)
			suspicious_pointer=@tempmem(0)
			readmem = *suspicious_pointer
	End Select
End Function

Sub writemem(ByVal addr As ULongInt, ByVal value As UByte) 'write memory
	If emulatorMode = "6502" Then
		cpu.memory(addr) = value
	Else
		Select Case addr
			Case &h2000 To &h3FFF
				writePPUreg(addr And &h2007, value)
			Case &h4014
			writePPUreg(&H4014,value)
			'Case &h4000 To &h4015, &h4017
				'apu stuff
			Case &h4016
				PadWrite
				'reset the read position
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
		Close #f
	EndIf
	Open ExePath & "\strawberry.ini" For Input As #f
	Input #f, screenx
	Input #f, screeny
	Input #f, opgoal
	Close #f
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

Sub CAE
	If strawberry Then ImageDestroy(Strawberry)
	Close
	End
End Sub

/'==============================================================================
                                       End subroutines
================================================================================'/
ScreenRes screenx,screeny,32,2
framebuffer = ImageCreate(screenx,screeny,RGB(0,0,0))
strawberry = freeimage_load_fb(CurDir & "/Res/SBNES.png", TRUE) ' load cute strawberry :)
initcpu
loadROM ' loadfile into ROM and cpu memory
Cls
If emulatorMode = "6502" Then cpu.pc = &h0600 ' set pc to program start for simple 6502 stuff
start = Timer
If logops = 1 Then Open "log.txt" For Output As #99
Do
	cpu.oldps = cpu.ps
	cpu.oldsp = cpu.sp
	cpu.oldacc = cpu.acc
	cpu.oldx = cpu.X
	cpu.oldy = cpu.y
	opsPerSecond = totalops / (Timer-start)
	'====================================REMOVE THIS======================================================
	If emulatormode = "6502" Then cpu.memory(&hfe) = Rnd*255 ' random number generator for simple 6502 programs
	'====================================REMOVE THIS======================================================
	keycheck
	cpu.oldpc = cpu.pc 'this is for storing debug information
	cpu.pc+=1
	totalops+=1
	decode_and_execute(cpu.memory(cpu.pc-1)) ' decode the binary located at the PC to opcode and address mode and then execute the instruction
	clear_b '==========================================================HACKS
	If logcomp = 1 Then comparelog '======================================================================
	If ticks >=85 And emulatorMode <> "6502" Then
		ppuLoop
		ticks = 0
	EndIf
	nextskip-=1
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
	End if
	status_timer+=1
	If status_timer >= OpsPerSecond/20 And emulatorMode = "6502" Then
		simplegraphics
		push_framebuffer
		status_timer = 0
	End If
	If opsPerSecond > opgoal Then Sleep 10
	If logops = 1 Then write_the_log
Loop While Not MultiKey(SC_ESCAPE)
Close
CAE

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
