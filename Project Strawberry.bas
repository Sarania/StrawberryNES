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
#Include Once "crt.bi" 'c runtime
#Include Once "file.bi" 'File functions
#Include Once "Freeimage.bi" ' Freeimage library
#Include Once "inc/freetofb.bi" 'Easily use Freeimage images in Freebasic
#Include Once "zlib.bi"
#Include Once "Inc/freetypeclass.bi" 'fontz
Declare Function readmem(ByVal addr As ULongInt, ByVal numbytes As UInteger = 1) As ULongInt ' for reading memory
Declare Sub writemem(ByVal addr As ULongInt, ByVal value As UByte) 'write a value to NES memory
Declare Sub fprint(ByVal x As Integer, ByVal y As Integer, ByVal text As String, ByVal c As Integer = RGB(255,255,255))'Print to the screen with a font
Declare Sub status 'print various status stuff to the screen
Declare Sub initcpu 'initialize the 6502
Declare Sub loadROM 'load a ROM in to memory
Declare Sub CAE 'Cleanup and exit
Declare Sub loadini 'Load the ini file
Declare Sub nmi
Declare Sub Write_The_log
Dim Shared As UByte debug
Dim Shared As UInteger opstoskip, nextskip, opGoal, ticks, romsize, screenx, screeny, starts, totalops, logops=1
Dim Shared As String opHistory(0 To 255), emulatorMode, instruction, amode, msg, version
Dim Shared As Single start, lastframetime,opsPerSecond, stepstart
Dim Shared As Any Ptr strawberry
Dim Shared As UInteger status_timer




Type cpus
	'------------------------'
	'   6502 Registers/MEM   '
	'------------------------'
	oldpc As UShort 'save pc for debug
	acc As UByte 'accumulator
	X As UByte 'X register
	Y As UByte 'Y register
	PS As UByte 'Processor status register, only updated in this byte for pushing and pulling it from the stack
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
	sp As UByte = &hFF 'stack pointer
	memory(0 To 65535) As UByte 'RAM
	'nesReset As Integer 'reset vector
End Type

Type ppus
	sprRAM (0 To &hFF) As UByte
	vram(0 To &hFFFF) As ubyte
	scanline As UInteger
	PPUCTRL As UByte
	PPUMASK As UByte
	PPUSTATUS As UByte
	OAMADDR As UByte
	OAMDATA As UByte
	PPUSCROLL As UByte
	PPUADDR As UByte
	PPUDATA As UByte
	OAMDMA As UByte
	sprAddr As UShort
	vrAddr As UShort
	#define  PPUCTRL_V        ( ppu.ppuctrl And 128 ) / 128
	#Define  PPUCTRL_P        ( ppu.ppuctrl And 64 ) / 64
	#Define  PPUCTRL_H        ( ppu.ppuctrl And 32 ) / 32
	#Define  PPUCTRL_B        ( ppu.ppuctrl And 16 ) / 16
	#Define  PPUCTRL_S        ( PPU.PPUCTRL And 8 ) / 8
	#Define  PPUCTRL_I        ( PPU.PPUCTRL And 4 ) / 4
	#Define  PPUCTRL_NN      (( PPU.PPUCTRL And 3 ) * &h400 ) + &h2000
	#Define PPUMASK_INTENSIFY_B    ( PPU.PPUMASK And 128 ) / 128
	#Define PPUMASK_INTENSIFY_G    ( PPU.PPUMASK And 64 ) / 64
	#Define PPUMASK_INTENSIFY_R    ( PPU.PPUMASK And 32 ) / 32
	#Define  PPUMASK_S       ( PPU.PPUMASK And 16 ) / 16
	#Define  PPUMASK_B       ( PPU.PPUMASK And 8 ) / 8
	#Define  PPUMASK_SL      ( PPU.PPUMASK And 4 ) / 4
	#Define  PPUMASK_BL      ( PPU.PPUMASK And 2 ) / 2
	#Define  PPUMASK_G       ( PPU.PPUMASK And 1 ) 
	#Define  PPUSTATUS_V     ( PPU.PPUSTATUS And 128 ) / 128
	#Define  PPUSTATUS_S     ( PPU.PPUSTATUS And 64 ) / 64
	#Define  PPUSTATUS_O     ( PPU.PPUSTATUS And 32 ) / 32 
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
Dim Shared cpu As cpus '6502 CPU
Dim Shared ppu As ppus 'suspicious PPU
Dim Shared header As headers
#Include Once "inc/loadrom.bi"
loadini ' need to load it here because of font stuff
ChDir ExePath
ChDir("..")

/'==============================================================================
                                       font stuff
================================================================================'/
Dim As Integer fonts = 18
'compute font based on screeny, sketchy but works reasonably well
fonts = CInt(screeny/32)
'but not smaller than 18
If fonts < 18 Then fonts = 18

'Load fonts
Dim Shared As truetype font
If font.init Then Stop
If font.get_font("res/arial.ttf")=0 Then Stop
font.set_render_mode(FT_RENDER_MODE_NORMAL)
font.set_screen_size(screenx,screeny)
font.set_size(fonts)
font.set_color(RGB(255,255,255))
font.set_back_color(RGB(0,0,0))
/'==============================================================================
                                     End font stuff
================================================================================'/
#Include Once "inc/misc.bi" 'misc stuff
#Include Once "inc/ppu.bi" 'PPU
#Include Once "inc/6502_instruction_set.bi" ' contains the instruction set
#Include Once "inc/decoder.bi" ' decodes hex opcodes to asm
emulatorMode = "6502"
lastframetime = Timer
version = "0.40 alpha"
debug = 1
opstoskip = 1
nextskip = 1

/'==============================================================================
                                       Subroutines
================================================================================'/

Sub status
	Dim blackout As Any Ptr
	blackout = ImageCreate(screenx/2,screeny,RGB(0,0,0))
	Put (1,1),blackout,PSet
	ImageDestroy(blackout)
	font.set_size 10
	fprint 1,15, "Emulator mode: " & emulatorMode
	fprint 1,25, "PRG size: " & header.prgSize*16 & " | " & header.prgSize*16*1024
	fprint 1,35, "Total ops: " & totalops & " | Stepping by: " & opstoskip & "                     "
	fprint 1,45, "Ops per second: " &  opsPerSecond & "                         "
	fprint 1,65, "Registers:                                           "
	fprint 1,75, "________________________               "
	fprint 1,85, "A: " & IIf(cpu.acc < &h10,"0" & Hex(cpu.acc),Hex(cpu.acc)) & " X: " & IIf(cpu.x < &h10,"0" & Hex(cpu.x),Hex(cpu.x)) & " Y: " & IIf(cpu.y < &h10,"0" & Hex(cpu.y),Hex(cpu.y)) & "                         "
	fprint 1,95, "PC: " & cpu.PC & " ($" & Hex(cpu.pc) & ")" & "                         "
	fprint 1,105, "Stack pointer: " & cpu.sp - &hff & "($" & Hex(cpu.sp-&hff) & ")" & "                         "
	Line(1,115)-(120,143),RGB(255,255,255),b
	fprint 3,125, "N   V   -   B   D   I   Z   C"
	Line (1,130)-(120,130),RGB(255,255,255)
	'fprint 3,140, cpu.flagS & "   " & cpu.flagV & "   " & cpu.flagU & "   " & cpu.flagB & "   " & cpu.flagD & "   " & cpu.flagI & "   " & cpu.flagZ & "   " & cpu.flagC
	fprint 3,140, Flag_S & "   " & Flag_V & "   " & flag_U & "   " & flag_B & "   " & flag_D & "   " & flag_I & "   " & flag_Z & "   " & flag_C
	For z As UByte = 0 To 6
		Line (12+(z*15),115)-(12+(z*15),143),RGB(255,255,255)
	Next
	fprint 1,165, "Processor status: " & cpu.ps & " " & " (" & Hex(cpu.ps) & ")"
	fprint 1,175, "Message: " & msg
	msg = "                                                                 "
	fprint 1,185, "Trace:"
	For i As Integer = 1 To 20
		fprint 1, 195+(i*10), opHistory(i) & "               "
	Next
	font.set_size 18
	fprint(2, screeny-55, "Project Strawberry",RGB(255,0,0))
	fprint(2, screeny-40, "Version 0.40 alpha ")
	fprint(2, screeny-25, "By Blyss Sarania and Nobbs66")
	Put(screenx-70,6),strawberry, Alpha
End Sub

Sub fprint(ByVal x As Integer, ByVal y As Integer, ByVal text As String, ByVal c As Integer = RGB(255,255,255)) 'print to screen with font
	font.set_color(c)
	font.print_text(x, y, text)
End Sub

Sub initcpu 'initialize CPU and RAM
	For i As Integer = 0 To 65535
		cpu.memory(i) = 0
	Next

	clear_s
	clear_z
	set_i
	Clear_d
	clear_c
	clear_v
	set_b
	set_u
	cpu.PS = &h34 'init status
	ppu.ppuctrl = 0 '00000000
	ppu.ppumask = 0 '00000000
	ppu.ppustatus = 160 '10100000
	ppu.oamaddr = 0 '00000000
	ppu.ppuscroll = 0 '00000000
	ppu.ppuaddr = 0 '00000000
	ppu.ppudata = 0 '00000000
End Sub

Sub nmi
	writemem((cpu.sp+&h100),(cpu.pc Shr 8))
	cpu.sp -=1
	writemem((cpu.sp+&h100), (cpu.pc And &hff))
	cpu.sp -=1
	writemem(cpu.sp+&h100,cpu.ps)
	cpu.sp -=1
	set_i
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

Function readmem(ByVal addr As ULongInt, ByVal numbytes As UInteger = 1) As ULongInt
	Select Case addr
		Case &h2000 To &h3FFF
			If addr = &h2002 Then cpu.memory(&h2002) And= 127
			readmem = readPPUreg(addr And &h2007)
		Case &h4015
			'apu stuff
		Case &h4016
			'controller stuff
		Case Else
			Dim As ULongInt Ptr suspicious_pointer
			Dim As UByte tempmem(0 To 7)
			For q As UByte = 0 To numbytes-1
				tempmem(q)=cpu.memory(addr+q)
			Next
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
			Case &h4000 To &h4015, &h4017
				'apu stuff
			Case &h4016
				'controller stuff
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
ScreenRes screenx,screeny,32
strawberry = freeimage_load_fb(CurDir & "/Res/strawberry.png", TRUE) ' load cute strawberry :)
initcpu
loadROM ' loadfile into ROM and cpu memory
Cls
If emulatorMode = "6502" Then cpu.pc = &h0600 ' set pc to program start for simple 6502 stuff
start = Timer
If logops = 1 Then Open "log.txt" For Output As #99
Do
	opsPerSecond = totalops / (Timer-start)
	cpu.memory(&hfe) = Rnd*255 ' random number generator for simple 6502 programs
	'====================================REMOVE THIS======================================================
	If totalops = 27000 Then cpu.memory(&h2002) = &h80 'Temporary tell the system that the PPU is warmed up


	'====================================REMOVE THIS======================================================
	keycheck
	cpu.oldpc = cpu.pc 'this is for storing debug information
	decode(cpu.memory(cpu.pc)) ' decode the binary located at the PC to opcode and address mode
	cpu.pc+=1
	totalops+=1
	Select Case instruction
		Case "ADC"
			INS_ADC
		Case "AND"
			INS_AND
		Case "ASL"
			INS_ASL
		Case "BCC"
			INS_BCC
		Case "BCS"
			INS_BCS
		Case "BEQ"
			INS_BEQ
		Case "BIT"
			INS_BIT
		Case "BMI"
			INS_BMI
		Case "BNE"
			INS_BNE
		Case "BPL"
			INS_BPL
		Case "BRK"
			INS_BRK
		Case "BVC"
			INS_BVC
		Case "BVS"
			INS_BVS
		Case "CLC"
			INS_CLC
		Case "CLD"
			INS_CLD
		Case "CLI"
			INS_CLI
		Case "CLV"
			INS_CLV
		Case "CMP"
			INS_CMP
		Case "CPX"
			INS_CPX
		Case "CPY"
			INS_CPY
		Case "DEC"
			INS_DEC
		Case "DEX"
			INS_DEX
		Case "DEY"
			INS_DEY
		Case "EOR"
			INS_EOR
		Case "INC"
			INS_INC
		Case "INX"
			INS_INX
		Case "INY"
			INS_INY
		Case "JMP"
			INS_JMP
		Case "JSR"
			INS_JSR
		Case "LDA"
			INS_LDA
		Case "LDX"
			INS_LDX
		Case "LDY"
			INS_LDY
		Case "LSR"
			INS_LSR
		Case "NOP"
			INS_NOP
		Case "ORA"
			INS_ORA
		Case "PHA"
			INS_PHA
		Case "PHP"
			INS_PHP
		Case "PLA"
			INS_PLA
		Case "PLP"
			INS_PLP
		Case "ROL"
			INS_ROL
		Case "ROR"
			INS_ROR
		Case "RTI"
			INS_RTI
		Case "RTS"
			INS_RTS
		Case "SBC"
			INS_SBC
		Case "SEC"
			INS_SEC
		Case "SED"
			INS_SED
		Case "SEI"
			INS_SEI
		Case "STA"
			INS_STA
		Case "STX"
			INS_STX
		Case "STY"
			INS_STY
		Case "TAX"
			INS_TAX
		Case "TAY"
			INS_TAY
		Case "TSX"
			INS_TSX
		Case "TXA"
			INS_TXA
		Case "TXS"
			INS_TXS
		Case "TYA"
			INS_TYA
		Case Else
			status
			Print "Decoder broken somehow. It received " & instruction
			Do
				Sleep 10
			Loop While InKey$ = ""
			Sleep
	End Select
	
	If ticks >=85 Then ppuLoop
	If ticks >=85 Then ticks = 0
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
		status
		simplegraphics
	EndIf
	status_timer+=1
	If status_timer >= OpsPerSecond/20  Then
		status
		simplegraphics
		status_timer=0
	End If
	If opsPerSecond > opgoal Then Sleep 10

	/'==============================================================================
                                       Sanity Checks
================================================================================'/

	If cpu.sp > 255 Or cpu.sp < 0 Then
		Cls
		Print "Stack pointer out of bounds!"
		Sleep
		CAE
	EndIf



	/'==============================================================================
                                       End sanity checks
================================================================================'/
If logops = 1 Then write_the_log
Loop While Not MultiKey(SC_ESCAPE)
Close
CAE