Declare Sub INS_ADC
Declare Sub INS_AND
Declare Sub INS_ASL
Declare Sub INS_BCC
Declare Sub INS_BCS
Declare Sub INS_BEQ
Declare Sub INS_BIT
Declare Sub INS_BMI
Declare Sub INS_BNE
Declare Sub INS_BPL
Declare Sub INS_BRK
Declare Sub INS_BVC
Declare Sub INS_BVS
Declare Sub INS_CLC
Declare Sub INS_CLD
Declare Sub INS_CLI
Declare Sub INS_CLV
Declare Sub INS_CMP
Declare Sub INS_CPX
Declare Sub INS_CPY
Declare Sub INS_DEC
Declare Sub INS_DEX
Declare Sub INS_DEY
Declare Sub INS_EOR
Declare Sub INS_INC
Declare Sub INS_INX
Declare Sub INS_INY
Declare Sub INS_JMP
Declare Sub INS_JSR
Declare Sub INS_LDA
Declare Sub INS_LDX
Declare Sub INS_LDY
Declare Sub INS_LSR
Declare Sub INS_NOP
Declare Sub INS_ORA
Declare Sub INS_PHA
Declare Sub INS_PHP
Declare Sub INS_PLA
Declare Sub INS_PLP
Declare Sub INS_ROL
Declare Sub INS_ROR
Declare Sub INS_RTI
Declare Sub INS_RTS
Declare Sub INS_SBC
Declare Sub INS_SEC
Declare Sub INS_SED
Declare Sub INS_SEI
Declare Sub INS_STA
Declare Sub INS_STX
Declare Sub INS_STY
Declare Sub INS_TAX
Declare Sub INS_TAY
Declare Sub INS_TSX
Declare Sub INS_TXA
Declare Sub INS_TXS
Declare Sub INS_TYA
Declare Sub get_data

Dim Shared As UShort taddr 'address with either data to be operated on, or the address to use itself
Dim Shared As byte Ptr tdata ' points at data to be operated on unless I fucked up the pointers again

Sub get_data
	Dim As UShort indaddr
	'this function will return the proper data to the opcodes depending on the addressing mode. Hopefully!
	Select Case amode
		Case "ACC" 'Accumulator
			tdata = @cpu.acc
		Case "IMM" 'Immediate
			tdata = @cpu.memory(cpu.pc)
			cpu.pc+=1
		Case "IMP" 'implied
			'do nothing
		Case "REL" 'relative
			tdata = @cpu.memory(cpu.pc)
			cpu.pc+=1
		Case "ABS" 'Absolute
			taddr = readmem(cpu.pc,2)
			tdata = @cpu.memory(taddr)
			cpu.pc+=2
		Case "ABSX" 'absolute X
			taddr = readmem(cpu.pc,2) + cpu.X
			tdata = @cpu.memory(taddr)
			cpu.pc+=2
		Case "ABSY" 'absolute Y
			taddr = readmem(cpu.pc,2) + cpu.y
			tdata = @cpu.memory(taddr)
			cpu.pc+=2
		Case "ZP" 'Zero page
			taddr = readmem(cpu.pc)
			taddr = taddr And &hff
			tdata = @cpu.memory(taddr)
			cpu.pc+=1
		Case "IND" 'Indirect
			taddr = readmem(cpu.pc,2)
			cpu.pc+=2
		Case "ZPX" 'Zero page X
			taddr = readmem(cpu.pc) + cpu.X
			taddr = taddr And &hff
			tdata = @cpu.memory(taddr)
			cpu.pc+=1
		Case "ZPY" 'Zero page Y
			taddr = readmem(cpu.pc) + cpu.y
			taddr = taddr And &hff
			tdata = @cpu.memory(taddr)
			cpu.pc+=1
		Case "INDX" 'Indirect X
			indaddr = readmem(cpu.pc)
			indaddr = (indaddr + cpu.X) And &hFF
			taddr = readmem(indaddr,2)
			tdata = @cpu.memory(taddr)
			cpu.pc+=1
		Case "INDY" 'indrectY
			indaddr = readmem(cpu.pc)
			taddr = readmem(indaddr,2)
			taddr+=cpu.y
			tdata = @cpu.memory(taddr)
			cpu.pc+=1
	End Select
	opHistory(0) = opHistory(0) & " " & taddr & " " & *tdata
End Sub
Sub INS_ADC
	'add with carry
	Dim As Integer adctmp
	get_data
	adctmp = (cpu.acc + *tdata + flag_c) And &hFF
	If Bit(cpu.acc,7) <> Bit(adctmp,7) Then set_v Else clear_v
	If Bit(cpu.acc,7) Then set_s Else clear_s
	If adctmp = 0 Then set_z Else clear_z
	If adctmp < cpu.acc Then set_c Else clear_c
	cpu.acc = adctmp And &hFF
End Sub

Sub INS_AND
	'accumulator AND memory
	get_data
	cpu.acc = cpu.acc And *tdata
	If cpu.acc = 0 Then set_z Else clear_z
	If Bit(cpu.acc,7) Then set_s Else clear_s
End Sub

Sub INS_ASL
	'shift left
	get_data
	If Bit(*tdata,7) Then set_c Else clear_c
	*tdata Shl = 1
	If Bit(*tdata,7) Then set_s Else clear_s
	If tdata = 0 Then set_z Else clear_z
End Sub

Sub INS_BCC
	'branch if carry clear
	get_data
	If flag_C = 0 Then cpu.pc+ = *tdata
End Sub

Sub INS_BCS
	'branch if carry set
	get_data
	If flag_c = 1 Then cpu.pc+= *tdata
End Sub

Sub INS_BEQ
	get_data
	If flag_z = 1 Then cpu.pc+= *tdata
End Sub

Sub INS_BIT
	Dim bittmp As UInteger
	get_data
	bittmp = cpu.acc And *tdata
	If Bit(*tdata,6) Then set_v Else clear_v
	If Bit(*tdata,7) Then set_s Else clear_s
	If bittmp = 0 Then set_z Else clear_z
End Sub

Sub INS_BMI
	get_data
	If flag_s = 1 Then cpu.pc+ = *tdata
End Sub

Sub INS_BNE
	get_data
	If flag_z = 0 Then cpu.pc+= *tdata
End Sub

Sub INS_BPL
	get_data
	If flag_s = 0 Then cpu.pc+ = *tdata
End Sub

Sub INS_BRK
	If emulatorMode = "6502" Then
		simplegraphics
		status
		font.set_size 10
		fprint (screenx/2) - 100, screeny/2, "Program issued break command! Halting.", RGB(0,0,255)
		font.set_size 18
		Sleep
		cae
	EndIf
	'Break
	cpu.PC += 1
	writemem(&h100+cpu.sp,cpu.acc)
	cpu.sp-=1
	writemem(&h100+cpu.sp,cpu.ps)
	cpu.sp-=1
	set_b
	cpu.PC = (cpu.memory(&hFFFF) Shl 8) Or cpu.memory(&hFFFE)
End Sub

Sub INS_BVC
	get_data
	If flag_v = 0 Then cpu.pc+ = *tdata
End Sub

Sub INS_BVS
	get_data
	If flag_v = 1 Then cpu.pc+ = *tdata
End Sub

Sub INS_CLC
	'clear carry flag
	clear_c
End Sub

Sub INS_CLD
	'clear dedimal flag
	clear_d
End Sub

Sub INS_CLI
	'clear interrupt flag
	clear_i
End Sub

Sub INS_CLV
	'clear overflow flag
	clear_v
End Sub

Sub INS_CMP
	'compare accumulator with memory
	get_data
	Dim As UByte cmptmp = cpu.acc - *tdata
	If cmptmp = 0 Then set_z Else clear_z
	If Bit(cmptmp,7) Then set_s Else clear_s
	If cmptmp >= 0 Then set_c Else clear_c
End Sub

Sub INS_CPX
	'compare X with memory
	get_data
	Dim As UByte cmptmp = cpu.x - *tdata
	If cmptmp = 0 Then set_z Else clear_z
	If Bit(cmptmp,7) Then set_s Else clear_s
	If cmptmp >= 0 Then set_c Else clear_c
End Sub

Sub INS_CPY
	'compare Y with memory
	get_data
	Dim As UByte cmptmp = cpu.y - *tdata
	If cmptmp = 0 Then set_z Else clear_z
	If Bit(cmptmp,7) Then set_s Else clear_s
	If cmptmp >= 0 Then set_c Else clear_c
End Sub

Sub INS_DEC
	'decrement memory
	get_data
	*tdata -= 1
	If *tdata = 0 Then set_z Else clear_z
	If Bit(*tdata,7) Then set_s Else clear_s
End Sub

Sub INS_DEX
	'Decrement X register
	cpu.X -=1
	If cpu.x = 0 Then set_z Else Clear_z
	If Bit(cpu.x,7) Then set_s Else clear_s
End Sub

Sub INS_DEY
	'Decrement Y register
	cpu.Y -=1
	If cpu.y = 0 Then set_z Else clear_z
	If Bit(cpu.y,7) Then set_s Else clear_s
End Sub

Sub INS_EOR
	'xor accumulator
	get_data
	cpu.acc = cpu.acc Xor *tdata
	If cpu.acc = 0 Then set_z Else clear_z
	If Bit(cpu.acc,7) Then set_s Else clear_s
End Sub

Sub INS_INC
	'increment memory
	get_data
	*tdata += 1
	If *tdata = 0 Then set_z Else clear_z
	If Bit(*tdata,7) Then set_s Else clear_s
End Sub

Sub INS_INX
	'Increment X register
	cpu.x+=1
	If cpu.x = 0 Then set_z Else clear_z
	If Bit(cpu.x,7) Then set_s Else clear_s

End Sub

Sub INS_INY
	'Increment Y register
	cpu.y+=1
	If cpu.y = 0 Then set_z Else clear_z
	If Bit(cpu.y,7) Then set_s Else clear_s
End Sub

Sub INS_JMP
	'jump
	get_data
	cpu.pc = taddr
End Sub

Sub INS_JSR
	'jump subroutine
	get_data
	writemem(&h100+cpu.sp,LoByte(cpu.pc))
	writemem(&h100+cpu.sp-1,HiByte(cpu.pc))
	cpu.sp-=2
	cpu.pc = taddr
End Sub

Sub INS_LDA
	'load accumulator
	get_data
	cpu.acc = *tdata
	If cpu.acc = 0 Then set_z Else clear_z
	If Bit(cpu.acc,7) Then set_s Else clear_s
End Sub

Sub INS_LDX
	'load x register
	get_data
	cpu.x = *tdata
	If cpu.x = 0 Then set_z Else clear_z
	If Bit(cpu.x,7) Then set_s Else clear_s
End Sub

Sub INS_LDY
	'load y register
	get_data
	cpu.y = *tdata
	If cpu.y = 0 Then set_z Else clear_z
	If Bit(cpu.y,7) Then set_s Else clear_s
End Sub

Sub INS_LSR
	'logical shift right
	get_data
	If Bit(*tdata,0) Then set_c Else clear_c
	*tdata Shr = 1
	If Bit(*tdata,7) Then set_s Else clear_s
	If *tdata = 0 Then set_z Else clear_z

End Sub

Sub INS_NOP
	'no operation
End Sub

Sub INS_ORA
	'or accumulator
	get_data
	cpu.acc = cpu.acc Or *tdata
	If cpu.acc = 0 Then set_z Else clear_z
	If Bit(cpu.acc,7) Then set_s Else clear_s
End Sub

Sub INS_PHA
	'push accumulator to stack
	writemem(&h100+cpu.sp,cpu.acc)
	cpu.sp-=1
End Sub

Sub INS_PHP
	'push processor status to stack
	writemem(&h100+cpu.sp,cpu.ps)
	cpu.sp-=1
End Sub

Sub INS_PLA
	'pull from stack to accumulator
	cpu.sp+=1
	cpu.acc = readmem(&h100+cpu.sp)
	If cpu.acc = 0 Then set_z Else clear_z
	If Bit(cpu.acc,7) Then set_s Else clear_s
End Sub

Sub INS_PLP
	'pull processor status from stack
	cpu.sp+=1
	cpu.ps = readmem(&h100+cpu.sp)
End Sub

Sub INS_ROL
	'rotate left
	'checked
	get_data
	Dim As UByte roltmp
	roltmp = *tdata
	*tdata Shl = 1 'shift whole thing left
	If flag_c = 1 Then *tdata = BitSet(*tdata,0) Else *tdata = BitReset(*tdata,0)'  put old carry bit in new bit 0
	If Bit(roltmp,7) Then set_c Else clear_c ' put old bit 7 in new carry bit
	If *tdata = 0 Then set_z Else clear_z
	If Bit(*tdata,7) Then set_s Else clear_s
End Sub

Sub INS_ROR
	'rotate right
	'checked
	get_data
	Dim As UByte roltmp
	roltmp = *tdata
	*tdata Shr = 1 'shift whole thing right
	If flag_c = 1 Then *tdata = BitSet(*tdata,7) Else *tdata = BitReset(*tdata,7)'  put old carry bit in new bit 7
	If Bit(roltmp,0) Then set_c Else clear_c ' put old bit 0 in new carry bit
	If *tdata = 0 Then set_z Else clear_z
	If Bit(*tdata,7) Then set_s Else clear_s
End Sub

Sub INS_RTI
	'return from interrupt
	cpu.sp+=1
	cpu.ps = readmem(&h100+cpu.sp)
	cpu.sp+=1
	cpu.pc = readmem(&h100+cpu.sp)
End Sub

Sub INS_RTS
	Dim suspicious_pointer As UShort Ptr
	Dim suspicious_array(0 To 1) As UByte
	cpu.sp+=1
	suspicious_array(1) = readmem(&h100+cpu.sp)
	cpu.sp+=1
	suspicious_array(0) = readmem(&h100+cpu.sp)
	suspicious_pointer = @suspicious_array(0)
	cpu.pc = *suspicious_pointer
End Sub

Sub INS_SBC
	'subtract with carry
	Dim As Integer sbctmp
	get_data
	sbctmp = cpu.acc - *tdata - (1 - flag_c)
	If sbctmp = 0 Then set_z Else clear_z
	If Bit(sbctmp,7) Then set_s Else clear_s
	If sbctmp >= 0 Then set_c Else clear_c
	If ((cpu.acc Xor sbctmp) And &H80) And ((cpu.acc Xor *tdata) And &H80) Then set_v Else clear_v
	cpu.acc = sbctmp And &Hff
End Sub

Sub INS_SEC
	'Set carry flag
	set_c
End Sub

Sub INS_SED
	'Set decimal flag
	set_d
End Sub

Sub INS_SEI
	'set interrupt flag
	set_i
End Sub

Sub INS_STA
	'store accumulator in memory
	get_data
	writemem(taddr,cpu.acc)
End Sub

Sub INS_STX
	'store x in memory
	get_data
	writemem(taddr,cpu.x)
End Sub

Sub INS_STY
	'store y in memory
	get_data
	writemem(taddr,cpu.y)
End Sub

Sub INS_TAX
	'transfer accumulator to X
	cpu.x = cpu.acc
	If cpu.x = 0 Then set_z Else clear_z
	If Bit(cpu.x,7) Then set_s Else clear_s
End Sub

Sub INS_TAY
	'transfer accumulator to Y
	cpu.y = cpu.acc
	If cpu.y = 0 Then set_z Else clear_z
	If Bit(cpu.y,7) Then set_s Else clear_s
End Sub

Sub INS_TSX
	'transfer stack pointer to x
	cpu.x = cpu.sp
	If cpu.x = 0 Then set_z Else clear_z
	If Bit(cpu.x,7) Then set_s Else clear_s
End Sub

Sub INS_TXA
	'transfer X to accumulator
	cpu.acc = cpu.x
	If cpu.acc = 0 Then set_z Else clear_z
	If Bit(cpu.acc,7) Then set_s Else clear_s
End Sub

Sub INS_TXS
	'transfer x to stack pointer
	cpu.sp = cpu.x
End Sub

Sub INS_TYA
	'transfer Y to accumulator
	cpu.acc = cpu.y
	If cpu.acc = 0 Then set_z Else clear_z
	If Bit(cpu.acc,7) Then set_s Else clear_s
End Sub