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
Dim Shared debug As UByte = 0 ' If debug is set, stepping mode is enabled and the emulation pauses after each opcode is executed
Dim Shared monitor As UByte = 1 ' If monitor is set you can see the debug infos, if not, the graphics take up the whole window
Dim Shared opGoal As UInteger ' Ops per second is limited to this number
#Include Once "crt.bi" 'C runtime functions
#Include Once "fbgfx.bi" 'Freebasic graphics library
Using fb ' Namespace
#Include Once "file.bi" 'File functions
#Include Once "Freeimage.bi" ' Freeimage library
#Include Once "inc/freetofb.bi" 'Easily use Freeimage images in Freebasic
#Include Once "zlib.bi"
#Include Once "Inc/freetypeclass.bi" 'fontz
Declare Function readmem(ByVal addr As LongInt, ByVal numbytes As UInteger = 1) As UInteger ' for reading memory
Declare Sub writemem(ByVal addr As LongInt, ByVal value As Byte) ' for writing memory
Declare Sub status ' debug infos
Declare Sub initcpu ' reset cpu
Declare Sub loadROM ' load ROM
Declare Sub CAE ' (C)leanup(A)nd(E)xit
Declare Sub fprint(ByVal x As Integer, ByVal y As Integer, ByVal text As String, ByVal c As Integer = RGB(255,255,255))
Declare Sub loadini
Declare Sub savestate
Declare Sub loadstate
Declare Sub NMI 'Non Maskable Interrrupt
Declare Sub IRQ 'Interrupt Requeset
Declare Sub vBlank 
Declare Sub DMA 'Direct Memory Access

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
	FlagS As UByte ' Sign flag
	FlagV As UByte ' Overflow flag
	flagU As UByte ' Unusued flag
	flagB As UByte ' Break flag
	flagD As UByte ' Decimal Flag
	flagI As UByte ' Interrupt Flag
	flagZ As UByte ' Zero flag
	flagC As UByte ' Carry flag
	PC As UShort 'program counter
	sp As UShort = 511 'stack pointer
	memory(0 To 65535) As Byte 'RAM
	resetV As Integer 'Reset Vector
	irqV As Integer 'IRQ Vector
	nmiV As Integer 'NMI Vevtor
	'stack = 256 - 511
	'---------------------------------'
	'          PPU Registers          '
	'---------------------------------'
   'PPU Control Register. 
   sprRAM (0 To &hFF) As Byte 'Sprite RAM
	VRAM (0 To 65535) As Byte 'PPU VRAM
	writeVRAM As Byte 'VRAM Pointer
	flipflop As Byte

End Type


Type headers
	signature(0 To 3) As Byte
	prgSize As Byte ' in 16KB pages
	chrSize As Byte ' in 8KB pages
	Flags6 As Byte
	Flags7 As Byte
	prgRAMSize As Byte ' in 8KB pages
	Flags9 As Byte
	flags10 As Byte
	zeros(0 To 4) As byte
End Type

ReDim Shared As Byte rom(0 To 1) 'ROM
ReDim Shared As Byte prgROM(0 To 1)
ReDim Shared As Byte chrROM(0 To 1)
ReDim Shared As Byte prgRAM(0 To 1)
Dim Shared As String opHistory(0 To 255)


Dim Shared header As headers
Dim Shared cpu As cpus
Dim Shared As String instruction, amode, msg, version
Dim Shared As UInteger ticks, romsize, screenx, screeny, start, totalops
Dim Shared As Single lastframetime
Dim Shared As Any Ptr strawberry
Dim Shared As String emulatorMode
emulatorMode = "6502"
lastframetime=Timer
version= "0.30 alpha"
#Include Once "inc/misc.bi" 'misc stuff
#Include Once "inc/6502_instruction_set.bi" ' contains the instruction set
#Include Once "inc/decoder.bi" ' decodes hex opcodes to asm

loadini ' need to load it here because of font stuff
ChDir ExePath
ChDir("..")

'font stuff
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

Sub fprint(ByVal x As Integer, ByVal y As Integer, ByVal text As String, ByVal c As Integer = RGB(255,255,255))
	font.set_color(c)
	font.print_text(x, y, text)
End Sub



Sub initcpu
	'initialize CPU and RAM
cpu.PS = &h34 
cpu.acc = 0 
cpu.X = 0    
cpu.Y = 0    
cpu.SP = &hFD 

For i As Integer = 0 to &hFFFF
cpu.memory(i) = 0
next
For i As Integer = 0 to &h7FF
cpu.memory(i) = &hFF
next
cpu.memory(&h0008) = &hF7
cpu.memory(&h0009) = &hEF
cpu.memory(&h000a) = &hDF
cpu.memory(&h000f) = &hBF
End Sub



Function readmem(ByVal addr As LongInt, ByVal numbytes As UInteger = 1) As UInteger
	'read from memory
	Dim As String strbytes
	For i As Integer = numbytes-1 To 0 Step -1
		strbytes = strbytes & Hex(cpu.memory(addr+i),2)
	Next
	'If addr = cpu.pc Then cpu.pc +=numbytes
	Return ValInt("&h" & strbytes)
End Function

Sub writemem(ByVal addr As LongInt, ByVal value As Byte)
	'write to memory
	cpu.memory(addr) = value
End Sub


Sub status
	Locate 1,1
	Print "Emulator mode: " & emulatorMode
	If emulatorMode = "NES" Then
		Print "PRG size: " & header.prgSize*16 & " | " & header.prgSize*16*1024 
	EndIf
	Print "Total ops: " & totalops
	Print "Ops per second: " &  CInt(totalops / (Timer-start)) & "                         "
	Print
	Print "Registers:                                           "
	Print "________________________               "
	Print "A: " & IIf(cpu.acc < &h10,"0" & Hex(cpu.acc),Hex(cpu.acc)) & " X: " & IIf(cpu.x < &h10,"0" & Hex(cpu.x),Hex(cpu.x)) & " Y: " & IIf(cpu.y < &h10,"0" & Hex(cpu.y),Hex(cpu.y)) & "                         "
	Print
	Print "PC: "; cpu.PC & " ($" & Hex(cpu.pc) & ")" & "                         "
	Print
	Print "Stack pointer: "; cpu.sp - &hff & "($" & Hex(cpu.sp-&hff) & ")" & "                         "
	Print
	Print "-----------------" & "                         "
	Print "|N|V|-|B|D|I|Z|C|" & "                         "
	Print "| | | | | | | | |"& "                         "
	Print "|" & cpu.flagS & "|" & cpu.flagV & "|" & cpu.flagU & "|" & cpu.flagB & "|" & cpu.flagD & "|" & cpu.flagI & "|" & cpu.flagZ & "|" & cpu.flagC & "|" & "                         "
	Print "|_______________|"   & "                         "
	Print
	Print "Message: "; msg & "  														"
	msg = "                                                                 "
	Print "                                                                             "
	Print "                                                                             "
	Print "Trace:"
	For i As Integer = 1 To 20
		Print opHistory(i) & "               "
	Next
	fprint(2, screeny-60, "Project Strawberry",RGB(255,0,0))
	fprint(2, screeny-35, "Version 0.30 alpha ")
	fprint(2, screeny-10, "By Blyss Sarania and Nobbs66")
	Put(screenx-70,6),strawberry, Alpha
End Sub

Sub loadini
	Dim f As Integer = FreeFile
	If Not FileExists(ExePath & "\strawberry.ini") Then
		Open ExePath & "\strawberry.ini" For Output As #f
		Print #f, 640
		Print #f, 480
		Print #f, 8000
		Close #f
	EndIf
	Open ExePath & "\strawberry.ini" For Input As #f
	Input #f, screenx
	Input #f, screeny
	Input #f, opgoal
	Close #f
End Sub

Sub loadROM
	Dim As String progname, shpname, onechr
	'See if we got a filename from the command line or drag and drop
	If Command(1) <> "" Then
		progname = Command(1)
		GoTo gotname
	End If
	Print "Note: ROM must be in EXEPATH, else use drag and drop to load it!)"
	Input "Program to run (compiled, no header): ", progname 'Get a filename from user
	progname = ExePath & "\" & progname

	gotname:
	If progname = "" Or Not FileExists(progname) Then 'Break if no such filename
		Cls
		Print "File not found: " & progname
		Sleep 3000
		CAE
	EndIf

	'remove path from filename
	For z As Integer = 1 To Len(progname) Step 1
		onechr = Right(Left(progname,z),1)
		If onechr = "\" Then
			onechr = ""
			shpname = ""
		EndIf
		shpname = shpname & onechr
	Next

	WindowTitle "Project Strawberry: " & shpname ' set window title
	Open progname For Binary As #1
	romsize = Lof(1)
	ReDim As Byte rom(0 To romsize) ' make ROM be the size of the... ROM
	For i As Integer = 0 To romsize 'Load ROM into ROM memory
		Get #1, i+1, rom(i), 1
	Next
	Close #1

	'read header
	open progname for binary as #1
	get #1, 1, header.signature()
	Get #1, 5, header.prgSize
	Get #1, 6, header.chrSize
	Get #1, 7, header.Flags6
	Get #1, 8, header.Flags7
	Get #1, 9, header.prgRAMSize
	Get #1, 10, header.Flags9
	Get #1, 11, header.flags10
	Get #1, 12, header.zeros()
	
	If Chr(header.signature(0)) = "N" And Chr(header.signature(1)) = "E" And Chr(header.signature(2)) = "S" Then
		ReDim As Byte PrgROM(header.prgSize*16*1024)
		ReDim As Byte chrROM(header.chrSize*8*1024)
		ReDim As Byte prgRAM(header.prgRAMSize*8*1024)
		Get #1, 17, prgROM()
		Get #1, 17 + header.prgSize, chrROM()
		emulatorMode = "NES"
	End If
	Close #1
	
	If emulatorMode= "6502" Then
	'copy rom to cpu memory
	For i As Integer = 0 To romsize
		cpu.memory(i+&h0600) = rom(i) ' yes this could overflow, this is just a temp setup!
	Next
	ElseIf emulatorMode = "NES" Then
		If header.prgSize*16*1024 > 32768 Then 
			Cls
			Print "ROM too big until mapper implemented"
			Sleep 2000,1
			cae
		EndIf
		Cls
		For i As Integer = 0 To header.prgSize*16*1024
			cpu.memory(&h8000+i) = prgRom(i)
		Next
		If header.prgSize*16*1024 = 16384 Then
			For i As Integer = 0 To header.prgSize*16*1024
				cpu.memory(&hc000+i) = prgRom(i)
			Next
		If header.chrSize*8*1024 = 8092 Then
			For i as Integer = 0 To header.chrSize*8*1024
				cpu.VRAM(i) = chrRom(i)
			Next
		EndIf
		EndIf
		cpu.resetV = cpu.memory(&hFFFD) And &hFF
		cpu.pc = (cpu.resetV Shl 8) Or (cpu.memory(&hFFFC)And &hFF)
	End if
End Sub
Sub IRQ
		cpu.irqV = cpu.memory(&hFFFF) And &hFF
		cpu.pc = (cpu.irqV Shl 8) Or (cpu.memory(&hFFFE)And &hFF)
End Sub
Sub NMI
	cpu.nmiV = cpu.memory(&hFFFB) And &hFF
	cpu.pc = (cpu.nmiV Shl 8) Or (cpu.memory(&hFFFA)And &hFF)	
End Sub
Sub savestate
	Dim As Integer f = FreeFile
	If FileExists("strawberry.state") Then Kill "strawberry.state"
	Open "strawberry.state" For Binary As #F
	Put #f, 1, cpu.memory()
	Put #f, 65537, cpu.acc
	Put #f, 65538, cpu.X
	Put #f, 65539, cpu.Y
	Put #f, 65540, cpu.ps
	Put #f, 65541, cpu.FlagS
	Put #f, 65542, cpu.FlagV
	Put #f, 65543, cpu.FlagU
	Put #f, 65544, cpu.FlagB
	Put #f, 65545, cpu.FlagD
	Put #f, 65546, cpu.FlagI
	Put #f, 65547, cpu.FlagZ
	Put #f, 65548, cpu.FlagC
	Put #f, 65549, cpu.sp
	Put #f, 65551, cpu.PC
	Close #f
	Print "Saved state as: "
	Print  CurDir & "/strawberry.state"
	Sleep 2000,1
End Sub

Sub loadstate
	Dim As Integer f = FreeFile
	Open "strawberry.state" For Binary As #F
	Get #1, 1, cpu.memory()
	Get #f, 65537, cpu.acc
	Get #f, 65538, cpu.X
	Get #f, 65539, cpu.Y
	Get #f, 65540, cpu.ps
	Get #f, 65541, cpu.FlagS
	Get #f, 65542, cpu.FlagV
	Get #f, 65543, cpu.FlagU
	Get #f, 65544, cpu.FlagB
	Get #f, 65545, cpu.FlagD
	Get #f, 65546, cpu.FlagI
	Get #f, 65547, cpu.FlagZ
	Get #f, 65548, cpu.FlagC
	Get #f, 65551, cpu.PC,2
	Get #f, 65549, cpu.sp,2
	Close #f
	Print "Loaded state."
	Sleep 1000,1
End Sub

Sub CAE
	If strawberry Then ImageDestroy(Strawberry)
	Close
	End
End Sub
Sub vBlank
cpu.flagI = 0 
cpu.irqV = cpu.memory(&hFFFF) And &hFF
cpu.pc = (cpu.irqV Shl 8) Or (cpu.memory(&hFFFE)And &hFF)
	
	
End Sub
Sub DMA
Dim dmatmp As Byte 
dmatmp = cpu.memory(&h4014) * 100 
For i As Integer = 0 To &hFF
	cpu.sprRAM(i) = cpu.memory(dmatmp+i)
Next
End Sub

ScreenRes screenx,screeny,32
strawberry = freeimage_load_fb(CurDir & "/Res/strawberry.png", TRUE) ' load cute strawberry :)
initcpu
loadROM ' loadfile into ROM and cpu memory
Cls

If debug > 0 Then
	If FileExists("log.txt") Then Kill ("log.txt") ' erase log so we can write a new one
EndIf

If emulatorMode = "6502" Then cpu.pc = &h0600 ' set program counter to program start

start = Timer ' for opcode timing

'main
Do
	keycheck
	cpu.oldpc = cpu.pc ' set this for storing debug information
	decode(cpu.memory(cpu.pc)) ' decode binary to opcode and address mode
	cpu.pc+=1
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
			Beep
			msg = "decoder broken somehow, received " & instruction
	End Select
	totalops+=1
	'///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	'												         PPU Execution Block 

	
	'PPU Reads/Writes are done through the PPU Control Registers at $2006/$2007
	
	'Update PPU Registers
	
	
	cpu.sprRAM(cpu.memory(&h2003)) = cpu.memory(&h20004)'SPR-RAM Write
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	'/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	'cpu.memory(&hfe) = CInt(Rnd*255) ' random number generator for simple 6502 programs

	If debug = 0 Then
		If (monitor = 1 And CInt(totalops / (Timer-start)) > opgoal) Or (totalops = 1 And monitor = 1) Then status
	Else
		status
	End If

	If Timer-lastframetime > 1/30  Then
		simplegraphics
		lastframetime= Timer
	End If

	If debug = 1 Then Sleep
	' check for keys

	If debug > 0 Then
		Open "log.txt" For Append As #22
		Print #22, Hex(cpu.oldpc,4) & ": " & instruction & " " & amode & " $" & Hex(taddr,4) & " $" & Hex(*tdata,2)
		Close #22
	EndIf

	If CInt(totalops / (Timer-start)) > opgoal Then Sleep 200 ' try to maintain goal ops per second
Loop While Not MultiKey(SC_ESCAPE)
Close


CAE