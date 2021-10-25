Sub assignPointers
	For i As UInteger = 0 To &hFFFF
		ppu.vram(i) = @ppu.vramdata(i)
		Select Case i
			Case &h3f10, &h3f14, &h3f18, &h3f1c'palette ram indexes
				ppu.vram(i) = @ppu.vramdata(i-&h10)
			Case &h3000 To &h3EFF 'mirror of 0x2000 to 0x2EFF
				ppu.vram(i) = @ppu.vramdata(i-&h1000)
			Case &h3F20 To &h3f2c 'mirror of palette ram indexes
				ppu.vram(i) = @ppu.vramdata(i-&h20)
			Case &h3F40 To &h3f4c 'mirror of palette ram indexes
				ppu.vram(i) = @ppu.vramdata(i-&h40)
			Case &h3F60 To &h3f6c 'mirror of palette ram indexes
				ppu.vram(i) = @ppu.vramdata(i-&h60)
			Case &h3F80 To &h3f8c
				ppu.vram(i) = @ppu.vramdata(i-&h80)
			Case &h3Fa0 To &h3fac
				ppu.vram(i) = @ppu.vramdata(i-&ha0)
			Case &h3Fb0 To &h3fbc
				ppu.vram(i) = @ppu.vramdata(i-&hb0)
			Case &h3Fe0 To &h3fec
				ppu.vram(i) = @ppu.vramdata(i-&he0)
		End Select
	Next

	Dim  ppuindex As UInteger
	Dim attribstuff As UInteger
	Dim aX As UByte, aY As ubyte
	If mirroring = "V" Then 'vertical mirroring
		For i As UInteger = &h2800 To &h2bff
			ppu.vram(i) = @ppu.vramdata(i-&h800)
			ppu.vram(i+&h400) = @ppu.vramdata(i-&h400)
		Next
		ppuindex = &h2000
		For nY As UByte = 0 To 29
			For nX As UByte = 0 To 31
				nameTable(nX, nY,0) = ppu.vram(ppuindex)
				attribstuff = &h2000 + &h3c0 + (nx\4) + ((nY\4)*8)
				nameTable(nX, nY,1) = ppu.vram(attribstuff)
				ppuindex+=1
			Next
		Next
		ppuindex = &h2400
		For nY As UByte = 0 To 29
			For nX As UByte = 32 To 63
				nameTable(nX, nY,0) = ppu.vram(ppuindex)
				attribstuff = &h2400 + &h3c0 + ((nx-32)\4) + ((nY\4)*8)
				nameTable(nX, nY,1) = ppu.vram(attribstuff)
				ppuindex+=1
			Next
		Next
		ppuindex = &h2000
		For nY As UByte = 30 To 59
			For nX As UByte = 0 To 31
				nameTable(nX, nY,0) = ppu.vram(ppuindex)
				attribstuff = &h2000 + &h3c0 + (nx\4) + (((nY-30)\4)*8)
				nameTable(nX, nY,1) = ppu.vram(attribstuff)
				ppuindex+=1
			Next
		Next
		ppuindex = &h2400
		For nY As UByte = 30 To 59
			For nX As UByte = 32 To 63
				nameTable(nX, nY,0) = ppu.vram(ppuindex)
				attribstuff = &h2400 + &h3c0 + ((nx-32)\4) + (((nY-30)\4)*8)
				nameTable(nX, nY,1) = ppu.vram(attribstuff)
				ppuindex+=1
			Next
		Next
	Else
		If mirroring = "H" Then 'horizontal mirroring
			For i As UInteger = &h2400 To &h27FF
				ppu.vram(i) = @ppu.vramdata(i-&h400)
				ppu.vram(i+&h800) = @ppu.vramdata(i+&h400)
			Next
			ppuindex = &h2000
			For nY As UByte = 0 To 29
				For nX As UByte = 0 To 31
					nameTable(nX, nY,0) = ppu.vram(ppuindex)
					attribstuff = &h2000 + &h3c0 + (nx\4) + ((nY\4)*8)
					nameTable(nX, nY,1) = ppu.vram(attribstuff)
					ppuindex+=1
				Next
			Next
			ppuindex = &h2000
			For nY As UByte = 0 To 29
				For nX As UByte = 32 To 63
					nameTable(nX, nY,0) = ppu.vram(ppuindex)
					attribstuff = &h2000 + &h3c0 + ((nx-32)\4) + ((nY\4)*8)
					nameTable(nX, nY,1) = ppu.vram(attribstuff)
					ppuindex+=1
				Next
			Next
			ppuindex = &h2800
			For nY As UByte = 30 To 59
				For nX As UByte = 0 To 31
					nameTable(nX, nY,0) = ppu.vram(ppuindex)
					attribstuff = &h2800 + &h3c0 + (nx\4) + (((nY-30)\4)*8)
					nameTable(nX, nY,1) = ppu.vram(attribstuff)
					ppuindex+=1
				Next
			Next
			ppuindex = &h2800
			For nY As UByte = 30 To 59
				For nX As UByte = 32 To 63
					nameTable(nX, nY,0) = ppu.vram(ppuindex)
					attribstuff = &h2800 + &h3c0 + ((nx-32)\4) + (((nY-30)\4)*8)
					nameTable(nX, nY,1) = ppu.vram(attribstuff)
					ppuindex+=1
				Next
			Next
		End If
	End if


End Sub
Sub loadMapper0 'NROM / CxROM
	For i As Integer = 0 To (header.prgSize*16*1024)-1
		cpu.memory(&h8000+i) = prgRom(i)
	Next
	If header.prgSize*16*1024 = 16384 Then
		For i As Integer = 0 To (header.prgSize*16*1024)-1
			cpu.memory(&hC000+i) = prgRom(i)
		Next
	End if
	For i as Integer = 0 To (header.chrSize*8*1024)-1
		*ppu.vram(i) = chrRom(i)
	Next
End Sub
Sub loadMapper1 'MMC1
	Dim As UInteger loadOffset
	loadOffset = header.prgSize*&h4000 'Number of banks x 16KB
	loadOffset -= &h4000 'Get start of last bank

	For i As Integer = 0 To &h3FFF
		cpu.memory(&h8000+i) = prgRom(i)
		cpu.memory(&hC000+i) = prgRom(loadOffset+i)
	Next
End Sub
Sub loadMapper3 'CxROM
	For i As Integer = 0 To &h7FFF
		cpu.memory(&h8000+i) = prgRom(i)
	Next
End Sub
Sub loadMapper2 'UxROM
	Dim As UInteger loadOffset
	loadOffset = header.prgSize*&h4000 'Number of banks x 16KB
	loadOffset -= &h4000 'Get start of last bank
	For i As Integer = 0 To &H3FFF '16K bank
		cpu.memory(&h8000+i) = prgRom(i) 'Load Bank 1 into slot 1
		cpu.memory(&HC000+i) = prgRom(loadOffset+i)
	Next
End Sub
Sub loadMapper4 'MMC3
	Dim As UInteger loadOffset
	loadOffset = header.prgSize * &h4000
	loadOffset -= &h4000
	For i As Integer = 0 To &h1FFF
		cpu.memory(&h8000+i) = prgRom(i)
		cpu.memory(&hA000+i) = prgRom(&h2000+i)
	Next
	For i as Integer = 0 To &h3FFF
		cpu.memory(&hC000+i) = prgRom(loadOffset + i)
	Next
End Sub
Sub loadMapper7 'AxROM
	For i As Integer = 0 To &h7FFF
		cpu.memory(&h8000+i) = prgRom(i)
	Next
End Sub
Sub loadROM
	Dim As String progname, shpname, onechr
	'See if we got a filename from the command line or drag and drop
	If Command(1) <> "" Then
		progname = Command(1)
		GoTo gotname
	End If
	Print "Note: ROM must be in EXEPATH, else use drag and drop to load it!)"
	Print "Press ENTER to load the last booted ROM!"
	Input "Program to run (compiled, no header): ", progname 'Get a filename from user

	If progname <> "" Then progname = ExePath & "\" & progname else progname = lastrom

	gotname:
	If progname = "" Or Not FileExists(progname) Then 'Break if no such filename
		Cls
		Print "File not found: " & progname
		Sleep 3000
		CAE
	EndIf
	lastrom = progname
	writeini

	'remove path from filename
	For z As Integer = 1 To Len(progname) Step 1
		onechr = Right(Left(progname,z),1)
		If onechr = "\" Then
			onechr = ""
			shpname = ""
			gamename = ""
		EndIf
		gamename = gamename & onechr
		shpname = shpname & onechr
	Next
	gamename = Left(gamename,Len(gamename)-4)

	WindowTitle "StrawberryNES - " & shpname ' set window title
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
	If Chr(header.signature(0)) = "N" And Chr(header.signature(1)) = "E" And Chr(header.signature(2)) = "S" Then
		Get #1, 5, header.prgSize
		Get #1, 6, header.chrSize
		Get #1, 7, header.Flags6
		Get #1, 8, header.Flags7
		Get #1, 9, header.prgRAMSize
		Get #1, 10, header.Flags9
		Get #1, 11, header.flags10
		Get #1, 12, header.zeros()
		ReDim As UByte PrgROM(header.prgSize*16*1024)
		ReDim As UByte chrROM(header.chrSize*8*1024)
		ReDim As UByte prgRAM(header.prgRAMSize*8*1024)
		Get #1, 17, prgROM()
		Get #1, 17 + header.prgSize*16*1024, chrROM()
		emulatorMode = "NES"
		skipread:
	End If
	Close #1
	If header.Flags6 And &b00000001 = 1 Then mirroring = "V" Else mirroring = "H"
	mapper = header.Flags6 Shr 4
	mapper Or= ((header.Flags7 Shr 4) And 4)
	If emulatorMode= "6502" Then
		'copy rom to cpu memory
		For i As Integer = 0 To romsize
			cpu.memory(i+&h0600) = rom(i) ' yes this could overflow, this is just a temp setup!
		Next
	ElseIf emulatorMode = "NES" Then
		Select Case mapper
			Case 0
				loadMapper0
			Case 1
				loadMapper1
			Case 2
				loadMapper2
			Case 3
				loadMapper3 'Mapper 0 loads the same as 3
			Case 4
				loadMapper4
			Case 7
				loadMapper7
			Case Else
				Beep
				Cls
				Print "Unsupported mapper"
				Sleep 2000
				Cls
				CAE
		End Select

	EndIf
	set_i
	assignPointers
	cpu.pc = (cpu.memory(&hFFFD) Shl 8) Or cpu.memory(&hFFFC)
	#ifdef debugmode
	'======================================================ONLY INCLUDED IF DEBUGMODE IS DEFINED!======================================================================
	If shpname = "autotest.nes" Then logcomp = 1 Else logcomp = 0
	If logcomp = 1 Then
		Dim dummy As String
		Open "nestest.log" For Input As #31
		cpu.pc = &HC000
	End If
	'==================================================================================================================================================================
	#EndIf
End Sub