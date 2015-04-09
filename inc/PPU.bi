'Picture Processing Unit core By Nobbs66
'Block of code sets up memory mapped PPU registers
'Maybe there is a faster option than just using a variable to write to a register
Sub INS_WritePPUCTRL1
	writemem(&h2000,LoByte,cpu.PPUctrol1)	
End Sub
Sub INS_WritePPUCTRL2
	writemem(&h2001,LoByte,cpu.PPUctrl2)
End Sub
Sub INS_WritePPUStatus
	writemem(&h2002,LoByte,cpu.PPUstatus)
End Sub
Sub INS_WriteSPRaddr
	writemem(&h2003,LoByte,cpu.SPRAddr)
End Sub
Sub INS_WritSPRio
	writemem(&h2004,LoByte,cpu.SPRIO)
End Sub
Sub INS_WriteVRAMaddr1
	writemem(&h2005,LoByte,cpu.VRAMaddr1)
End Sub
Sub INS_WriteVRAMaddr2
	writemem(&h2006,LoByte,cpu.VRAMaddr2)
End Sub
Sub INS_WriteVRAMio
	writemem(&h2007,LoByte,cpu.VRAMio)
End Sub

