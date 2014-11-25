Declare Sub decode(ByVal opc As Byte)

Sub decode(ByVal opc As Byte)
	Dim decodertemp As String
	decodertemp = Hex(opc)
	Select Case LCase(decodertemp)
		'add with carry
		Case "69"
			instruction = "ADC"
			amode = "IMM"

		Case "65"
			instruction = "ADC"
			amode = "ZP"

		Case "75"
			instruction = "ADC"
			amode = "ZPX"

		Case "6d"
			instruction = "ADC"
			amode = "ABS"

		Case "7d"
			instruction = "ADC"
			amode = "ABSX"

		Case "79"
			instruction = "ADC"
			amode = "ABSY"

		Case "61"
			instruction = "ADC"
			amode = "INDX"

		Case "71"
			instruction = "ADC"
			amode = "INDY"

			'logical and
		Case "29"
			instruction = "AND"
			amode =  "IMM"

		Case "25"
			instruction = "AND"
			amode = "ZP"

		Case "35"
			instruction = "AND"
			amode = "ZPX"

		Case "2d"
			instruction = "AND"
			amode = "ABS"

		Case "3d"
			instruction = "AND"
			amode = "ABSX"

		Case "39"
			instruction = "AND"
			amode = "ABSY"

		Case "21"
			instruction = "AND"
			amode = "INDX"

		Case "31"
			instruction = "AND"
			amode = "INDY"

			'arithmetic shift left
		Case "a"
			instruction = "ASL"
			amode = "ACC"

		Case "6"
			instruction = "ASL"
			amode = "ZP"

		Case "16"
			instruction = "ASL"
			amode = "ZPX"

		Case "e"
			instruction = "ASL"
			amode = "ABS"

		Case "1e"
			instruction = "ASL"
			amode = "ABSX"

			'branch if carry clear
		Case "90"
			instruction = "BCC"
			amode = "REL"

			'branch if carry set
		Case "b0"
			instruction = "BCS"
			amode = "REL"

			'branch if equal
		Case "f0"
			instruction = "BEQ"
			amode = "REL"

			'Bit test
		Case "24"
			instruction = "BIT"
			amode = "ZP"

		Case "2c"
			instruction = "BIT"
			amode = "ABS"

			'Branch if minus
		Case "30"
			instruction = "BMI"
			amode = "REL"

			'Branch if not equal
		Case "d0"
			instruction = "BNE"
			amode = "REL"

			'Branch if positve
		Case "10"
			instruction = "BPL"
			amode = "REL"

			'Break
		Case "0"
			instruction = "BRK"
			amode = "IMP"

			'Branch if overflow clear
		Case "50"
			instruction = "BVC"
			amode = "REL"

			'Branch if overflow set
		Case "70"
			instruction = "BVS"
			amode = "REL"

			'clear carry flag
		Case "18"
			instruction = "CLC"
			amode = "IMP"

			'clear decimal mode
		Case "d8"
			instruction = "CLD"
			amode = "IMP"

			'Clear interrupt disable
		Case "58"
			instruction = "CLI"
			amode = "IMP"

			'clear overflow flag
		Case "b8"
			instruction = "CLV"
			amode = "IMP"

			'compare
		Case "c9"
			instruction = "CMP"
			amode = "IMM"

		Case "c5"
			instruction = "CMP"
			amode = "ZP"

		Case "d5"
			instruction = "CMP"
			amode = "ZPX"

		Case "cd"
			instruction = "CMP"
			amode = "ABS"

		Case "dd"
			instruction = "CMP"
			amode = "ABSX"

		Case "d9"
			instruction = "CMP"
			amode ="ABSY"

		Case "c1"
			instruction = "CMP"
			amode = "INDX"

		Case "d1"
			instruction = "CMP"
			amode = "INDY"

			'compare x register
		Case "e0"
			instruction = "CPX"
			amode = "IMM"

		Case "e4"
			instruction = "CPX"
			amode = "ZP"

		Case "ec"
			instruction = "CPX"
			amode = "ABS"

			'compare Y register
		Case "c0"
			instruction = "CPY"
			amode = "IMM"

		Case "c4"
			instruction = "CPY"
			amode = "ZP"

		Case "cc"
			instruction = "CPY"
			amode = "ABS"

			'Decrement memory
		Case "c6"
			instruction = "DEC"
			amode = "ZP"

		Case "d6"
			instruction  = "DEC"
			amode = "ZPX"

		Case "ce"
			instruction = "DEC"
			amode = "ABS"

		Case "de"
			instruction = "DEC"
			amode = "ABSX"

			'Decrement X register
		Case "ca"
			instruction = "DEX"
			amode = "IMP"

			'decrement Y register
		Case "88"
			instruction = "DEY"
			amode = "IMP"

			'exclusive or
		Case "49"
			instruction = "EOR"
			amode = "IMM"

		Case "45"
			instruction = "EOR"
			amode = "ZP"

		Case "55"
			instruction = "EOR"
			amode = "ZPX"

		Case "4d"
			instruction = "EOR"
			amode = "ABS"

		Case "5d"
			instruction = "EOR"
			amode = "ABSX"

		Case "59"
			instruction = "EOR"
			amode = "ABSY"

		Case "41"
			instruction = "EOR"
			amode = "INDX"

		Case "51"
			instruction = "EOR"
			amode = "INDY"

			'increment memory
		Case "e6"
			instruction = "INC"
			amode = "ZP"

		Case "f6"
			instruction = "INC"
			amode = "ZPX"

		Case "ee"
			instruction = "INC"
			amode = "ABS"

		Case "fe"
			instruction = "INC"
			amode = "ABSX"

			'increment x register
		Case "e8"
			instruction = "INX"
			amode = "IMP"

			'increment y register
		Case "c8"
			instruction = "INY"
			amode = "IMP"

			'jump
		Case "4c"
			instruction = "JMP"
			amode = "ABS"

		Case "6c"
			instruction = "JMP"
			amode = "IND"

			'jump to subroutine
		Case "20"
			instruction = "JSR"
			amode = "ABS"

			'load accumulator
		Case "a9"
			instruction = "LDA"
			amode = "IMM"

		Case "a5"
			instruction = "LDA"
			amode = "ZP"

		Case "b5"
			instruction = "LDA"
			amode = "ZPX"

		Case "ad"
			instruction = "LDA"
			amode = "ABS"

		Case "bd"
			instruction = "LDA"
			amode = "ABSX"

		Case "b9"
			instruction = "LDA"
			amode = "ABSY"

		Case "a1"
			instruction = "LDA"
			amode = "INDX"

		Case "b1"
			instruction = "LDA"
			amode = "INDY"

			'Load X register
		Case "a2"
			instruction = "LDX"
			amode = "IMM"

		Case "a6"
			instruction = "LDX"
			amode = "ZP"

		Case "b6"
			instruction = "LDX"
			amode = "ZPY"

		Case "ae"
			instruction = "LDX"
			amode = "ABS"

		Case "be"
			instruction = "LDX"
			amode = "ABSY"

			'load Y register
		Case "a0"
			instruction = "LDY"
			amode = "IMM"

		Case "a4"
			instruction = "LDY"
			amode = "ZP"

		Case "b4"
			instruction = "LDY"
			amode = "ZPX"

		Case "ac"
			instruction = "LDY"
			amode = "ABS"

		Case "bc"
			instruction = "LDY"
			amode = "ABSX"

			'logical shift right
		Case "4a"
			instruction = "LSR"
			amode = "ACC"

		Case "46"
			instruction = "LSR"
			amode = "ZP"

		Case "56"
			instruction = "LSR"
			amode = "ZPX"

		Case "4e"
			instruction = "LSR"
			amode = "ABS"

		Case "5e"
			instruction = "LSR"
			amode = "ABSX"

			'no operation
		Case "ea"
			instruction = "NOP"
			amode = "IMP"

			'logical inclusive or
		Case "9"
			instruction = "ORA"
			amode = "IMM"

		Case "5"
			instruction = "ORA"
			amode = "ZP"

		Case "15"
			instruction = "ORA"
			amode = "ZPX"

		Case "d"
			instruction = "ORA"
			amode = "ABS"

		Case "1d"
			instruction = "ORA"
			amode = "ABSX"

		Case "19"
			instruction = "ORA"
			amode = "ABSY"

		Case "1"
			instruction = "ORA"
			amode = "INDX"

		Case "11"
			instruction = "ORA"
			amode = "INDY"

			'push accumulator
		Case "48"
			instruction = "PHA"
			amode = "IMP"

			'push processor status
		Case "8"
			instruction = "PHP"
			amode = "IMP"

			'pull accumulator
		Case "68"
			instruction = "PLA"
			amode = "IMP"

			'pull processor status
		Case "28"
			instruction = "PLP"
			amode = "IMP"

			'Rotate left
		Case "2a"
			instruction = "ROL"
			amode = "ACC"

		Case "26"
			instruction = "ROL"
			amode = "ZP"

		Case "36"
			instruction = "ROL"
			amode = "ZPX"

		Case "2e"
			instruction = "ROL"
			amode = "ABS"

		Case "3e"
			instruction = "ROL"
			amode = "ABSX"

			'rotate right
		Case "6a"
			instruction = "ROR"
			amode = "ACC"

		Case "66"
			instruction = "ROR"
			amode = "ZP"

		Case "76"
			instruction = "ROR"
			amode = "ZPX"

		Case "6e"
			instruction = "ROR"
			amode = "ABS"

		Case "7e"
			instruction = "ROR"
			amode = "ABSX"

			'return from interrupt
		Case "40"
			instruction = "RTI"
			amode = "IMP"

			'return from subroutine
		Case "60"
			instruction = "RTS"
			amode= "IMP"

			'subtract with carry
		Case "e9"
			instruction = "SBC"
			amode = "IMM"

		Case "e5"
			instruction = "SBC"
			amode = "ZP"

		Case "f5"
			instruction = "SBC"
			amode = "ZPX"

		Case "ed"
			instruction = "SBC"
			amode = "ABS"

		Case "fd"
			instruction = "SBC"
			amode = "ABSX"

		Case "f9"
			instruction = "SBC"
			amode = "ABSY"

		Case "e1"
			instruction = "SBC"
			amode = "INDX"

		Case "f1"
			instruction = "SBC"
			amode = "INDY"

			'set carry flag
		Case "38"
			instruction = "SEC"
			amode = "IMP"

			'set decimal flag
		Case "f8"
			instruction = "SED"
			amode = "IMP"

			'set interrupt disable
		Case "78"
			instruction = "SEI"
			amode = "IMP"

			'Store accumulator
		Case "85"
			instruction = "STA"
			amode = "ZP"

		Case "95"
			instruction = "STA"
			amode = "ZPX"

		Case "8d"
			instruction = "STA"
			amode = "ABS"

		Case "9d"
			instruction = "STA"
			amode = "ABSX"

		Case "99"
			instruction = "STA"
			amode = "ABSY"

		Case "81"
			instruction = "STA"
			amode = "INDX"

		Case "91"
			instruction = "STA"
			amode = "INDY"

			'store x register
		Case "86"
			instruction = "STX"
			amode = "ZP"

		Case "96"
			instruction = "STX"
			amode = "ZPY"

		Case "8e"
			instruction = "STX"
			amode = "ABS"

			'store y register
		Case "84"
			instruction = "STY"
			amode = "ZP"

		Case "94"
			instruction = "STY"
			amode = "ZPX"

		Case "8c"
			instruction = "STY"
			amode = "ABS"

			'transfer accumulator to X
		Case "aa"
			instruction = "TAX"
			amode = "IMP"

			'transfer accumulator to Y
		Case "a8"
			instruction = "TAY"
			amode = "IMP"

			'transfer stack pointer to X
		Case "ba"
			instruction = "TSX"
			amode = "IMP"

			'transfer X to accumulator
		Case "8a"
			instruction = "TXA"
			amode = "IMP"

			'transfer x to stack pointer
		Case "9a"
			instruction = "TXS"
			amode = "IMP"

			'transfer Y to accumulator
		Case "98"
			instruction = "TYA"
			amode = "IMP"

		Case Else
        instruction = "Decoder error! " & decodertemp
        amode = "Decoder error!"
        
	End Select
End Sub
