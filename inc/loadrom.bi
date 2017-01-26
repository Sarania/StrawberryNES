
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
	ReDim As UByte rom(romsize) ' make ROM be the size of the... ROM
	For i As Integer = 0 To romsize-1 'Load ROM into ROM memory
		Get #1, i+1, rom(i), 1
	Next
	Close #1

	'read header
	Open progname for binary as #1
	get #1, 1, header.signature()
	If Chr(header.signature(0)) <> "N" And Chr(header.signature(1)) <> "E" And Chr(header.signature(2)) <> "S" Then GoTo skipread
	Get #1, 5, header.prgSize
	Get #1, 6, header.chrSize
	Get #1, 7, header.Flags6
	Get #1, 8, header.Flags7
	Get #1, 9, header.prgRAMSize
	Get #1, 10, header.Flags9
	Get #1, 11, header.flags10
	Get #1, 12, header.zeros()
   skipread:
	If Chr(header.signature(0)) = "N" And Chr(header.signature(1)) = "E" And Chr(header.signature(2)) = "S" Then
		ReDim As UByte PrgROM(header.prgSize*16*1024)
		ReDim As UByte chrROM(header.chrSize*8*1024)
		ReDim As UByte prgRAM(header.prgRAMSize*8*1024)
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
		For i As Integer = 0 To (header.prgSize*16*1024)-1
			cpu.memory(32768+i) = prgRom(i)
		Next
		If header.prgSize*16*1024 = 16384 Then
			For i As Integer = 0 To (header.prgSize*16*1024)-1
				cpu.memory(49152+i) = prgRom(i)
			Next
		End if
		If header.chrSize*8*1024 = 8192 Then
			For i as Integer = 0 To (header.chrSize*8*1024)-1
				ppu.VRAM(i) = chrRom(i)
			Next
		EndIf
		set_i
		cpu.pc = (cpu.memory(&hFFFD) Shl 8) Or cpu.memory(&hFFFC)	
	End if
End Sub