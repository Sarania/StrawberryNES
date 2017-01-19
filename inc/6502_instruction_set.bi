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
Dim Shared As Byte Ptr tdata ' points at data to be operated on unless I fucked up the pointers again

Sub get_data
	Dim As Short indaddr
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
	adctmp = cpu.acc + *tdata + cpu.flagC
	adctmp = adctmp And &hFF
	If Bit(cpu.acc,7) <> Bit(adctmp,7) Then cpu.flagV = 1 Else cpu.flagV = 0
	If Bit(cpu.acc,7) Then cpu.FlagS = 1 Else cpu.FlagS = 0
	If adctmp = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If adctmp < cpu.acc Then cpu.flagC = 1 Else cpu.flagC = 0
	cpu.acc = adctmp And &hFF
End Sub

Sub INS_AND
	'checked
	'accumulator AND memory
	get_data
	cpu.acc = cpu.acc And *tdata
	If cpu.acc = 0 Then cpu.flagZ = 1 Else cpu.FlagZ = 0
	If Bit(cpu.acc,7) Then cpu.FlagS = 1 Else cpu.FlagS = 0
End Sub

Sub INS_ASL
	'shift left
	'checked
	get_data
	If Bit(*tdata,7) Then cpu.flagC = 1 Else cpu.FlagC = 0
	*tdata Shl = 1
	If Bit(*tdata,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	If tdata = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
End Sub

Sub INS_BCC
	'checked
	get_data
	If cpu.flagC = 0 Then cpu.pc+ = *tdata
End Sub

Sub INS_BCS
	'checked
	get_data
	If cpu.flagC = 1 Then cpu.pc+= *tdata
End Sub

Sub INS_BEQ
	'checked
	get_data
	If cpu.flagZ = 1 Then cpu.pc+= *tdata
End Sub

Sub INS_BIT
	'checked
	'and WTF does simply not comparing cpu.acc AND *tdata to 0 not work
	' how can this condition fail? "If cpu.acc AND *tdata = 0 or if cpu.acc AND *tdata != 0
	Dim bittmp As UInteger
	get_data
	bittmp = cpu.acc And *tdata
	If Bit(*tdata,6) Then cpu.flagV = 1 Else cpu.flagV = 0
	If Bit(*tdata,7) Then cpu.FlagS = 1 Else cpu.flagS = 0
	If bittmp = 0 Then cpu.FlagZ = 1 Else cpu.flagZ = 0
End Sub

Sub INS_BMI
	'checked
	get_data
	If cpu.flagS = 1 Then cpu.pc+ = *tdata
End Sub

Sub INS_BNE
	'checked
	get_data
	If cpu.flagZ = 0 Then cpu.pc+= *tdata
End Sub

Sub INS_BPL
	'checked
	get_data
	If cpu.flagS = 0 Then cpu.pc+ = *tdata
End Sub

Sub INS_BRK
	'Break
	cpu.ps = BitSet(cpu.ps,4)
	
	'Print "Program Issued BRK command!"
	'Print instruction
	'Print amode
	'Print Hex(cpu.pc)
	cpu.pc = (cpu.memory(&hFFFF) Shl 8)Or cpu.memory(&hFFFE)
	'Sleep 1000,1
	'Sleep
	'Sleep
	'cae
	
 	
	ticks+=7
End Sub

Sub INS_BVC
	'checked
	get_data
	If cpu.flagV = 0 Then cpu.pc+ = *tdata
End Sub

Sub INS_BVS
	'checked
	get_data
	If cpu.flagV = 1 Then cpu.pc+ = *tdata
End Sub

Sub INS_CLC
	'checked
	'clear carry flag
	cpu.flagC = 0
	ticks+=2
End Sub

Sub INS_CLD
	'clear dedimal flag
	'checked
	cpu.FlagD = 0
	ticks+=2
End Sub

Sub INS_CLI
	'clear interrupt flag
	'checked
	cpu.flagI = 0
	ticks+=2
End Sub

Sub INS_CLV
	'clear overflow flag
	'checked
	cpu.flagV = 0
	ticks+=2
End Sub

Sub INS_CMP
	'compare accumulator with memory
	'possibly broken
	get_data
	Dim As UByte cmptmp = cpu.acc - *tdata
	If cmptmp = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cmptmp,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	If cmptmp >= 0 Then cpu.flagC = 1 Else Cpu.flagC = 0
End Sub

Sub INS_CPX
	'compare X with memory
	'possibly broken
	get_data
	Dim As UByte cmptmp = cpu.x - *tdata
	If cmptmp = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cmptmp,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	If cmptmp >= 0 Then cpu.flagC = 1 Else Cpu.flagC = 0
End Sub

Sub INS_CPY
	'compare Y with memory
	'possibly broken
	get_data
	Dim As UByte cmptmp = cpu.y - *tdata
	If cmptmp = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cmptmp,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	If cmptmp >= 0 Then cpu.flagC = 1 Else Cpu.flagC = 0
End Sub

Sub INS_DEC
	'decrement memory
	get_data
	*tdata -= 1
	If *tdata = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(*tdata,7) Then cpu.flagS = 1 Else cpu.flagS = 0

End Sub

Sub INS_DEX
	'Decrement X register
	cpu.X -=1
	If cpu.x = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.x,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	ticks+=2
End Sub

Sub INS_DEY
	'Decrement Y register
	cpu.Y -=1
	If cpu.y = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.y,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	ticks+=2
End Sub

Sub INS_EOR
	'xor accumulator
	get_data
	cpu.acc = cpu.acc Xor *tdata
	If cpu.acc = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.acc,7) Then cpu.flagS = 1 Else cpu.flagS = 0
End Sub

Sub INS_INC
	'increment memory
	get_data
	*tdata += 1
	If *tdata = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(*tdata,7) Then cpu.flagS = 1 Else cpu.flagS = 0
End Sub

Sub INS_INX
	'Increment X register
	cpu.x+=1
	If cpu.x = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.x,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	ticks+=2
End Sub

Sub INS_INY
	'Increment Y register
	cpu.y+=1
	If cpu.y = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.y,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	ticks+=2
End Sub

Sub INS_JMP
	'jump
	get_data
	cpu.pc = taddr
End Sub

Sub INS_JSR
	'jump subroutine
	get_data
	writemem(cpu.sp,LoByte(cpu.pc))
	writemem(cpu.sp-1,HiByte(cpu.pc))
	cpu.sp-=2
	cpu.pc = taddr
End Sub

Sub INS_LDA
	'load accumulator
	get_data
	cpu.acc = *tdata
	If cpu.acc = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.acc,7) Then cpu.flagS = 1 Else cpu.flagS = 0
End Sub

Sub INS_LDX
	'load x register
	get_data
	cpu.x = *tdata
	If cpu.x = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.x,7) Then cpu.flagS = 1 Else cpu.flagS = 0

End Sub

Sub INS_LDY
	'load y register
	get_data
	cpu.y = *tdata
	If cpu.y = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.y,7) Then cpu.flagS = 1 Else cpu.flagS = 0
End Sub

Sub INS_LSR
	'logical shift right
	'checked
	get_data
	If Bit(*tdata,0) Then cpu.flagC = 1 Else cpu.flagC = 0
	*tdata Shr = 1
	If Bit(*tdata,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	If *tdata = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0

End Sub

Sub INS_NOP
	'checked
	'no operation
	ticks+=2
End Sub

Sub INS_ORA
	'or accumulator
	get_data
	cpu.acc = cpu.acc Or *tdata
	If cpu.acc = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.acc,7) Then cpu.flagS = 1 Else cpu.flagS = 0

End Sub

Sub INS_PHA
	'push accumulator to stack
	writemem(cpu.sp,cpu.acc)
	cpu.sp-=1
	ticks+=3
End Sub

Sub INS_PHP
	'push processor status to stack
	If cpu.flagC = 1 Then cpu.ps = BitSet(cpu.ps,0) Else cpu.ps = BitReset(cpu.ps,0)
	If cpu.flagZ = 1 Then cpu.ps = BitSet(cpu.ps,1) Else cpu.ps = BitReset(cpu.ps,1)
	If cpu.flagI = 1 Then cpu.ps = BitSet(cpu.ps,2) Else cpu.ps = BitReset(cpu.ps,2)
	If cpu.flagD = 1 Then cpu.ps = BitSet(cpu.ps,3) Else cpu.ps = BitReset(cpu.ps,3)
	If cpu.flagB = 1 Then cpu.ps = BitSet(cpu.ps,4) Else cpu.ps = BitReset(cpu.ps,4)
	If cpu.flagU = 1 Then cpu.ps = BitSet(cpu.ps,5) Else cpu.ps = BitReset(cpu.ps,5)
	If cpu.flagV = 1 Then cpu.ps = BitSet(cpu.ps,6) Else cpu.ps = BitReset(cpu.ps,6)
	If cpu.flagS = 1 Then cpu.ps = BitSet(cpu.ps,7) Else cpu.ps = BitReset(cpu.ps,7)
	writemem(cpu.sp,cpu.ps)
	cpu.sp-=1
	ticks+=3
End Sub

Sub INS_PLA
	'pull from stack to accumulator
	cpu.sp+=1
	cpu.acc = readmem(cpu.sp)
	If cpu.acc = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.acc,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	ticks+=4
End Sub

Sub INS_PLP
	'pull processor status from stack
	cpu.sp+=1
	cpu.ps = readmem(cpu.sp)
	If Bit(cpu.ps,0) Then cpu.flagC = 1 Else cpu.flagC = 0
	If Bit(cpu.ps,1) Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.ps,2) Then cpu.flagI = 1 Else cpu.flagI = 0
	If Bit(cpu.ps,3) Then cpu.flagD = 1 Else cpu.flagD = 0
	If Bit(cpu.ps,4) Then cpu.flagB = 1 Else cpu.flagB = 0
	If Bit(cpu.ps,5) Then cpu.flagU = 1 Else cpu.flagU = 0
	If Bit(cpu.ps,6) Then cpu.flagV = 1 Else cpu.flagV = 0
	If Bit(cpu.ps,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	ticks+=4
End Sub

Sub INS_ROL
	'rotate left
	'checked
	get_data
	Dim As UByte roltmp
	roltmp = *tdata
	*tdata Shl = 1 'shift whole thing left
	If cpu.flagC = 1 Then *tdata = BitSet(*tdata,0) Else *tdata = BitReset(*tdata,0)'  put old carry bit in new bit 0
	If Bit(roltmp,7) Then cpu.flagC = 1 Else cpu.flagC = 0 ' put old bit 7 in new carry bit
	If *tdata = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(*tdata,7) Then cpu.flagS = 1 Else cpu.flagS = 0
End Sub

Sub INS_ROR
	'rotate right
	'checked
	get_data
	Dim As UByte roltmp
	roltmp = *tdata
	*tdata Shr = 1 'shift whole thing right
	If cpu.flagC = 1 Then *tdata = BitSet(*tdata,7) Else *tdata = BitReset(*tdata,7)'  put old carry bit in new bit 7
	If Bit(roltmp,0) Then cpu.flagC = 1 Else cpu.flagC = 0 ' put old bit 0 in new carry bit
	If *tdata = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(*tdata,7) Then cpu.flagS = 1 Else cpu.flagS = 0
End Sub

Sub INS_RTI
	'return from interrupt
	cpu.sp+=1
	cpu.ps = readmem(cpu.sp)
	cpu.sp+=1
	cpu.pc = readmem(cpu.sp)
	ticks+=6
End Sub

Sub INS_RTS
	Dim hbyte As UByte
	Dim lbyte As UByte
	'return from subroutine
	cpu.sp+=1
	LByte = readmem(cpu.sp)
	cpu.sp+=1
	hbyte = readmem(cpu.sp)
	cpu.pc = ValInt("&h" & Hex(lbyte,2) & Hex(hbyte,2))
	ticks+=6
End Sub

Sub INS_SBC
	'subtract with carry
	'maybe right
	Dim As Integer sbctmp
	get_data
	sbctmp = cpu.acc - *tdata - (1 - cpu.flagC)
	If sbctmp = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(sbctmp,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	If sbctmp >= 0 Then cpu.flagC = 1 Else cpu.flagC = 0
	cpu.flagV=IIf((((cpu.acc Xor sbctmp) And &H80) And ((cpu.acc Xor *tdata) And &H80)),1,0)
	cpu.acc = sbctmp And &Hff
End Sub

Sub INS_SEC
	'checked
	'Set carry flag
	cpu.flagC = 1
	ticks+=2
End Sub

Sub INS_SED
	'checked
	'Set decimal flag
	cpu.flagD = 1
	ticks+=2
End Sub

Sub INS_SEI
	'checked
	'set interrupt flag
	cpu.flagI = 1
	ticks+=2
End Sub

Sub INS_STA
	'checked
	'store accumulator in memory
	get_data
	writemem(taddr,cpu.acc)
End Sub

Sub INS_STX
	'checked
	'store x in memory
	get_data
	writemem(taddr,cpu.x)
End Sub

Sub INS_STY
	'checked
	'store y in memory
	get_data
	writemem(taddr,cpu.y)
End Sub

Sub INS_TAX
	'transfer accumulator to X
	cpu.x = cpu.acc
	If cpu.x = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.x,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	ticks+=2
End Sub

Sub INS_TAY
	'transfer accumulator to Y
	cpu.y = cpu.acc
	If cpu.y = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.y,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	ticks+=2
End Sub

Sub INS_TSX
	'transfer stack pointer to x
	cpu.x = cpu.sp
	If cpu.x = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.x,7) Then cpu.flagS = 1 Else cpu.flagS = 0
End Sub

Sub INS_TXA
	'transfer X to accumulator
	cpu.acc = cpu.x
	If cpu.acc = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.acc,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	ticks+=2
End Sub

Sub INS_TXS
	'transfer x to stack pointer
	cpu.sp = cpu.x
End Sub

Sub INS_TYA
	'transfer Y to accumulator
	cpu.acc = cpu.y
	If cpu.acc = 0 Then cpu.flagZ = 1 Else cpu.flagZ = 0
	If Bit(cpu.acc,7) Then cpu.flagS = 1 Else cpu.flagS = 0
	ticks+=2
End Sub