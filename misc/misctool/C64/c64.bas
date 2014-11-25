' c64.bas

#if __FB_DEBUG__
# define dprint(msg) open err for output as #99:print #99,"debug: " & msg:close #99
#else
# define dprint(msg):
#endif


type MEMORY
  public:
  declare constructor
  declare destructor
  declare function ReadByte   (adr as ushort) as byte
  declare function ReadUByte  (adr as ushort) as ubyte
  declare function ReadUShort (adr as ushort) as ushort
  declare sub      WriteByte  (adr as ushort,b8 as byte)
  declare sub      WriteUByte (adr as ushort,b8 as ubyte)
  declare sub      WriteUShort(adr as ushort,w16 as ushort)

  private:
  declare function Peek8   (adr as integer) as ubyte
  declare sub      Poke8   (adr as integer,v as ubyte)

  as integer os_end     = &HFFFF
  as integer os_base    = &HE000
  as integer io_end     = &HDFFF
  as integer flopy_end  = &HDFFF
  as integer flopy_base = &HDF00
  as integer cpm_end    = &HDEFF
  as integer cpm_base   = &HDE00
  as integer cia2_end   = &HDDFF
  as integer cia2_base  = &HDD00
  as integer cia1_end   = &HDCFF
  as integer cia1_base  = &HDC00
  as integer col_end    = &HDBFF
  as integer col_base   = &HD800
  as integer sid_end    = &HD7FF
  as integer sid_base   = &HD400
  as integer vic_end    = &HD3FF
  as integer vic_base   = &HD000
  as integer io_base    = &HD000
  as integer basic_end  = &HBFFF
  as integer basic_base = &HA000

  as ubyte   mem64 (&HFFFE) ' Ram
  as ubyte   kernal(&H1FFF) ' OS
  as ubyte   basic (&H1FFF) ' Basic
  as ubyte   char  (&H07FF) ' Font
  as ubyte   col   (&H03E7) ' color triples
end type



enum ADR_MODES
_UNK ' unknow
_IMP ' instruction only
_IMM ' 1 byte operand (immidate)
' opr = mem(pc)
_ABS ' 2 byte lo hi
' adr = mem(pc) + mem(pc+1)*256
_ZERO  ' 1 byte lo    (zero page hi=0)
' adr = mem(pc) and 255
_ZEROX ' 1 byte lo    (zero page x hi=0)
' adr = (mem(pc)+x) and 255
_ZEROY ' 1 byte lo    (zero page y hi=0)
' adr = (mem(pc)+y) and 255
_ABSX  ' 2 byte lo hi (abs x)
' adr = mem(pc ) + mem(pc+1)*256 + x
_ABSY  ' 2 byte lo hi (abs y)
' adr = mem(pc ) + mem(pc+1)*256 + y
_REL   ' 1 byte lo    (rel. branch -128 - +127)
' adr= PC + lo
_INDX  ' 1 byte lo (ind x)
' adr =(mem(pc )+x) and 255
' adr = mem(adr) + mem(adr+1)*256
_INDY  ' 2 byte lo hi (ind y)
' adr = mem(pc ) + mem(pc +1)*256 + y
_IND   ' 2 byte lo hi (jmp indirect)
' adr = mem(pc ) + mem(pc +1)*256
' pc  = mem(adr) + mem(adr+1)*256
end enum

type FLAGS
  as ubyte  C:1
  as ubyte  Z:1
  as ubyte  I:1
  as ubyte  D:1
  as ubyte  B:1
  as ubyte  H:1
  as ubyte  V:1
  as ubyte  N:1
end type

type CPU6510_T as CPU6510 ptr

  type MULTI
    union
      as ushort u16
      as  short s16
      type
        union
          as ubyte ulo
          as  byte slo
        end union
        union
          as ubyte uhi
          as  byte shi
        end union
      end type
    end union
  end type

  type OPCODE
    as ubyte       code
    as zstring * 4 nam
    as integer     adrmode,bytes,ticks
    as MULTI       op
    as sub(byval lpCPU as CPU6510_T) decode
  end type

  type CPU6510
    public:
    declare constructor(mem  as MEMORY ptr)
    declare destructor
    declare operator CAST      as string
    declare function Tick(flg  as integer=&H7FFFFFFF) as integer
    declare function ADR_IMM   as ushort
    declare function ADR_REL   as ushort
    declare function ADR_ZERO  as ushort
    declare function ADR_ZEROX as ushort
    declare function ADR_ZEROY as ushort
    declare function ADR_ABS   as ushort
    declare function ADR_ABSX  as ushort
    declare function ADR_ABSY  as ushort
    declare function ADR_IND   as ushort
    declare function ADR_INDX  as ushort
    declare function ADR_INDY  as ushort
    declare function ADR_UNK   as ushort ' unknow
    declare sub      Push   (v as ubyte)
    declare function Pull      as ubyte

    union ' status register P
      as ubyte P
      as FLAGS F
    end union
    union ' accumulator A
      as ubyte   A ' A unsigned
      as  byte  sA ' A signed
    end union
    union ' index register X
      as ubyte   X ' X unsigned
      as  byte  sX ' X signed
    end union
    union ' index register Y
      as ubyte   Y ' X unsigned
      as  byte  sY ' X signed
    end union
    union ' program counter PC
      as ushort PC
      type
        as ubyte PL ' as lo hi bytes
        as ubyte PH
      end type
    end union
    union ' stack pointer
      as ushort SP
      type
        as ubyte S     ' as lo bytes
        as ubyte MSB   ' msb allways hi
      end type
    end union
    as MEMORY ptr mem
    as OPCODE     code
    private:
    as OPCODE Opcodes(255)
    as string StrAdrModes(12)
  end type

  type OLdSchool
    public:
    declare constructor
    declare destructor
    as MEMORY  ptr MEM
    as CPU6510 ptr CPU
  end type

  COLOR_ROM:
  data &H19191d,&Hfcfcf9,&H4c933a,&Hfab6fa
  data &Hedd27d,&H6f6acf,&Hd84f44,&H8bfbfb
  data &H5bd89c,&H077f53,&H9fef83,&H535757
  data &Ha7a3a7,&Hbfb7fb,&Hffa397,&He7efe9

  constructor OLdSchool
  dim as integer i,c
  dprint("OldSchool()")
  screenres 320+8*8,200+8*8,,8
  for i=0 to 15
    read c:palette i,c
  next
  line (0,0)-(319+8*8,199+8*8),3,bf
  mem=new MEMORY
  cpu=new CPU6510(mem)
  end constructor

  destructor OLdSchool
  delete CPU
  delete MEM
  dprint("OldSchool~")
  sleep 1000
  end destructor

  constructor MEMORY
  dim as integer i
  ' init all ROM's
  restore KERNAL_ROM
  for i=0 to 8191:read kernal(i):next
  restore BASIC_ROM
  for i=0 to 8191:read basic(i):next
  restore CHAR_ROM
  for i=0 to 2047:read char(i):next
  poke8(0,255):poke8(1,255)
  end constructor

  destructor MEMORY
  dprint("MEMORY~")
  end destructor

function MEMORY.peek8(adr as integer) as ubyte
    select case adr
    case &HE000 to &HFFFF:return kernal(adr-&HE000)
    case &HA000 to &HBFFF:return basic (adr-&HA000)
    case &HD800 to &HDBFF:return col   (adr-&HD800)
    case &HD000 to &HD3FF
    dim as integer reg=adr and &H003f
    if reg=&H12 then return 0 else return &HFF
    case else : return mem64(adr)
    end select
end function



sub MEMORY.poke8(adr as integer,v as ubyte)
  mem64(adr)=v
  if adr>=&HD800 and adr<=&HDBFF then
    adr-=&HD800:col(adr)=v
    adr+=1024:v=mem64(adr)
  end if

  select case adr
  case 1024 to 2023
  adr-=1024
  dim as integer b,c=v:c shl=3
  dim as integer xs=adr mod 40:xs shl =3:xs+=8*4
  dim as integer ys=adr  \  40:ys shl =3:ys+=8*4
  screenlock
  for y as integer = 0 to 7
    for x as integer = 0 to 7
      if char(c) and (128 shr x) then
        pset(xs+x,ys+y),3
      else
        pset(xs+x,ys+y),col(adr)
      end if
    next
    c+=1
  next
  screenunlock ys,ys+8
  end select
end sub


function MEMORY.ReadUByte(adr as ushort) as ubyte
  return peek8(adr)
end function
function MEMORY.ReadByte(adr as ushort) as byte
  return peek8(adr)
end function
function MEMORY.ReadUShort(adr as ushort) as ushort
  return peek8(adr) or peek8(adr+1) shl 8
end function

sub MEMORY.WriteByte(adr as ushort,b8 as byte)
  poke8(adr,b8)
end sub
sub MEMORY.WriteUByte(adr as ushort,b8 as ubyte)
  poke8(adr,b8)
end sub
sub MEMORY.WriteUShort(adr as ushort,w16 as ushort)
  poke8(adr,LOBYTE(w16)):poke8(adr+1,HIBYTE(w16))
end sub

constructor CPU6510(lpMem as memory ptr)
dprint("CPU6510()")
mem=lpMem
restore INSTRUCTION_SET
'opcode,name,adrmode,ticks,operand,decoder
for i as integer=0 to 255
  with Opcodes(i)
  read .code,.nam,.adrmode,.bytes,.ticks,.decode
  end with
next
restore ADDRESS_MODES
for i as integer=0 to 12
  read StrAdrModes(i)
next
' direction and data port
'mem->WriteUByte(0,&H27)
'mem->WriteUByte(1,&HE7)
' flags
'F.H=1
' stack pointer
MSB=1 ':S=&HFF
' reset vector
PC=&HFCE2
end constructor

destructor CPU6510
dprint("CPU6510~")
end destructor

operator CPU6510.CAST as string
return "PC:" & hex(PC,4) & _
" A:" & hex(A ,2) & _
" X:" & hex(X ,2) & _
" Y:" & hex(Y ,2) & _
" S:" & hex(S ,2) & _
"   N:" & F.N & _
" V:" & F.V & _
" -"  & _
" B:" & F.B & _
" D:" & F.D & _
" I:" & F.I & _
" Z:" & F.Z & _
" C:" & F.C
end operator

function CPU6510.Tick(flg as integer=&H7FFFFFFF) as integer
  static as integer Ticks
  dim as string msg
  dim as MULTI v
  ' get next opcode
  code=opcodes(mem->readubyte(PC))

  ' clear union
  code.op.u16=0
  Ticks+=1

  #if __FB_DEBUG__
  if flg=Ticks then

    dprint("tick: flag=1")

    msg   = Ticks & chr(13,10)
    msg & =  "A:" & hex(A,2) & _
    " X:" & hex(X,2) & _
    " Y:" & hex(Y,2) & _
    " S:" & hex(S,2) & _
    " P:" & bin(P,8) & chr(13,10)

    msg & = HEX(pc,4) & " " & hex(code.code,2) & " " & code.nam & " " & stradrmodes(code.adrmode)
  end if
  #endif

  PC+=1
  select case as const code.adrmode
    case _UNK
      #if __FB_DEBUG__
      dprint(msg & chr(13,10))
      PL=mem->readubyte(&HFFFC)
      PH=mem->readubyte(&HFFFD)
      beep:sleep:end
      #endif
    case _IMP
      #if __FB_DEBUG__
      if flg=Ticks then
        dprint(msg & chr(13,10))
        sleep
      endif
      #endif
      code.decode(@this)
    case _IMM
      #if __FB_DEBUG__
      if flg=Ticks then
        v.ulo=mem->readubyte(pc)
        dprint(msg & " #$" & hex(v.ulo,2) & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_IMM()
      code.decode(@this)
    case _ABS
      #if __FB_DEBUG__
      if flg=Ticks then
        v.u16=mem->readushort(pc)
        dprint(msg & "  $" & hex(v.u16,4) & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_ABS()
      code.decode(@this)
    case _ZERO
      #if __FB_DEBUG__
      if flg=Ticks then
        v.ulo=mem->readubyte(pc)
        dprint(msg & " $" & hex(v.ulo,2) & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_ZERO()
      code.decode(@this)
    case _ZEROX
      #if __FB_DEBUG__
      if flg=Ticks then
        v.ulo=mem->readubyte(pc)
        dprint(msg & " $" & hex(v.ulo,2) & ",X" & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_ZEROX()
      code.decode(@this)
    case _ZEROY
      #if __FB_DEBUG__
      if flg=Ticks then
        v.ulo=mem->readubyte(pc)
        dprint(msg & " $" & hex(v.ulo,2) & ",Y" & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_ZEROY()
      code.decode(@this)
    case _ABSX
      #if __FB_DEBUG__
      if flg=Ticks then
        v.u16=mem->readushort(pc)
        dprint(msg & " $" & hex(v.u16,4) & ",X" & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_ABSX()
      code.decode(@this)
    case _ABSY
      #if __FB_DEBUG__
      if flg=Ticks then
        v.u16=mem->readushort(pc)
        dprint(msg & " $" & hex(v.u16,4) & ",Y" & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_ABSY()
      code.decode(@this)
    case _REL
      #if __FB_DEBUG__
      if flg=Ticks then
        v.u16 =pc
        v.s16+=mem->ReadByte(pc)+1
        dprint(msg & " $" & hex(v.u16,4) & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_REL()
      code.decode(@this)
    case _INDX
      #if __FB_DEBUG__
      if flg=Ticks then
        v.u16=mem->ReadUShort(pc)
        dprint(msg & " ($" & hex(v.u16,4) & ",X)" & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_INDX()
      code.decode(@this)
    case _INDY
      #if __FB_DEBUG__
      if flg=Ticks then
        v.ulo=mem->ReadUByte(pc)
        dprint(msg & " ($" & hex(v.ulo,4) & "),Y" & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_INDY()
      code.decode(@this)
    case _IND
      #if __FB_DEBUG__
      if flg=Ticks then
        v.u16=mem->ReadUShort(pc)
        dprint(msg & " ($" & hex(v.u16,4) & ")" & chr(13,10))
        sleep
      endif
      #endif
      code.op.u16=ADR_IND()
      code.decode(@this)
  end select
  return 0
end function
'
' 6510 address modes
'
function CPU6510.ADR_UNK as ushort
  #if _FB_DEBUG__
  dprint("! adr unknow !")
  beep:sleep:end
  #endif
  sleep:return 0
end function

function CPU6510.ADR_IMM as ushort ' 1 byte #$xx
  ' mem(pc)
  function = PC
  PC+=1
end function

function CPU6510.ADR_REL as ushort  ' 1 byte (rel. branch -128 - +127)
  function=PC
  PC+=1
end function

function CPU6510.ADR_ABS as ushort  ' 2 byte $xx:xx
  ' adr = mem(pc) + mem(pc+1)*256
  function = mem->ReadUShort(pc)
  pc+=2
end function

function CPU6510.ADR_ZERO as ushort ' 1 byte $00:xx
  ' adr = mem(pc) and 255
  function = mem->ReadUByte(pc) and &HFF
  pc+=1
end function

function CPU6510.ADR_ZEROX as ushort' 1 byte 00:xx,x
  ' adr = (mem(pc)+x) and 255
  function = (mem->ReadUByte(pc)+x) and &HFF
  pc+=1
end function

function CPU6510.ADR_ZEROY as ushort' 1 byte 00:xx,y
  ' adr = (mem(pc)+y) and 255
  function = (mem->ReadUByte(pc)+y) and &HFF
  pc+=1
end function

function CPU6510.ADR_ABSX as ushort ' 2 byte $xx:xx,x
  ' adr = mem(pc ) + mem(pc+1)*256 + x
  function  = mem->ReadUShort(PC) + X
  PC+=2
end function

function CPU6510.ADR_ABSY as ushort ' 2 byte $xx:xx,y
  ' adr = mem(pc ) + mem(pc+1)*256 + y
  function = mem->ReadUShort(PC) + Y
  PC+=2
end function

function CPU6510.ADR_INDX as ushort ' 1 byte ($XX,x)
  ' adr =(mem(pc )+x) and 255
  ' adr = mem(adr) + mem(adr+1)*256
  dim as MULTI v
  v.u16=(mem->ReadUByte(pc)+x) and &HFF
  v.u16=mem->ReadUShort(v.u16)
  pc+=1
  return v.u16
end function

function CPU6510.ADR_INDY as ushort ' 1 byte ($XX),y
  ' v.ulo=mem->ReadUByte(pc)
  ' adr = mem(pc ) + mem(pc +1)*256 + y
  dim as MULTI v
  v.u16=mem->ReadUshort(mem->ReadUByte(PC))
  v.u16+=y
function = v.u16
    pc+=1
end function

function CPU6510.ADR_IND as ushort ' 2 byte ($xx:xx)
  ' adr = mem(pc ) + mem(pc +1)*256
  ' pc  = mem(adr) + mem(adr+1)*256
  dim as MULTI v
  v.u16=mem->ReadUShort(pc)
  v.u16=mem->ReadUShort(v.u16)
  pc+=2
  return v.u16
end function

sub CPU6510.Push(b as ubyte)
  mem->WriteUByte(sp,b)
  s-=1
end sub

function CPU6510.PULL as ubyte
  s+=1
  return mem->ReadUbyte(sp)
end function

'
' 6510 instructions
'
sub INS_UNK(lpCPU as CPU6510_T)
  #if __FB_DEBUG__
  dprint("! unk")
  beep:sleep:end
  #endif
end sub

sub INS_ADC(lpCPU as CPU6510_T)
  dim as MULTI v
  dim as ubyte ub
  ub=lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  v.u16=lpCPU->A + ub
  if lpCPU->F.c=1 then v.u16+=1
  lpCPU->F.v=iif(((not (lpCPU->A xor    ub) and &H80) and _
  (    (lpCPU->A xor v.ulo) and &H80)),1,0)
  lpCPU->A=v.ulo
  lpCPU->F.c=iif(v.u16>255,1,0)
  lpCPU->F.z=iif(v.ulo=0,1,0)
  lpCPU->F.n=iif(v.slo<0,1,0)
end sub

sub INS_AND(lpCPU as CPU6510_T)
  lpCPU->A=lpCPU->A and lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  lpCPU->F.z=iif(lpCPU->A =0,1,0)
  lpCPU->F.n=iif(lpCPU->sA<0,1,0)
end sub

sub INS_ASL(lpCPU as CPU6510_T)
  dim as MULTI v
  v.ulo=lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  lpCPU->F.c = iif(v.ulo and &H80,1,0)
  v.ulo shl = 1
  lpCPU->mem->WriteUbyte(lpCPU->Code.op.u16,v.ulo)
  lpCPU->F.z=iif(v.ulo=0,1,0)
  lpCPU->F.n=iif(v.slo<0,1,0)
end sub

sub INS_ASLA(lpCPU as CPU6510_T) ' ac
  lpCPU->F.c = iif(lpCPU->A and &H80,1,0)
  lpCPU->A shl = 1
  lpCPU->F.z=iif(lpCPU->A =0,1,0)
  lpCPU->F.n=iif(lpCPU->sA<0,1,0)
end sub

sub INS_BCC(lpCPU as CPU6510_T)
  if lpCPU->F.c=0 then
    dim as MULTI v
    v.u16 =lpCPU->pc
    v.s16-=1
    v.s16+=lpCPU->mem->ReadByte(lpCPU->Code.op.u16)+1
    lpCPU->pc=v.u16
  end if
end sub

sub INS_BCS(lpCPU as CPU6510_T)
  if lpCPU->F.c then
    dim as MULTI v
    v.u16 =lpCPU->pc
    v.s16-=1
    v.s16+=lpCPU->mem->ReadByte(lpCPU->Code.op.u16)+1
    lpCPU->pc=v.u16
  end if
end sub

sub INS_BEQ(lpCPU as CPU6510_T)
  if lpCPU->F.z=1 then
    dim as MULTI v
    v.u16 =lpCPU->pc
    v.s16-=1
    v.s16+=lpCPU->mem->ReadByte(lpCPU->Code.op.u16)+1
    lpCPU->pc=v.u16
  end if
end sub

sub INS_BIT(lpCPU as CPU6510_T)
  dim as byte b
  b=lpCPU->mem->Readbyte(lpCPU->Code.op.u16)
  lpCPU->F.n=iif(b and &H80,1,0)
  lpCPU->F.v=iif(b and &H40,1,0)
  lpCPU->F.z=iif(0=(b and lpCPU->sX),1,0)
end sub

sub INS_BMI(lpCPU as CPU6510_T)
  if lpCPU->F.n then
    dim as MULTI v
    v.u16 =lpCPU->pc
    v.s16-=1
    v.s16+=lpCPU->mem->ReadByte(lpCPU->Code.op.u16)+1
    lpCPU->pc=v.u16
  end if
end sub

sub INS_BNE(lpCPU as CPU6510_T)
  if lpCPU->F.z=0 then
    dim as MULTI v
    v.u16 =lpCPU->pc
    v.s16-=1
    v.s16+=lpCPU->mem->ReadByte(lpCPU->Code.op.u16)+1
    lpCPU->pc=v.u16
  end if
end sub

sub INS_BPL(lpCPU as CPU6510_T)
  if lpCPU->F.n=0 then
    dim as MULTI v
    v.u16 =lpCPU->pc
    v.s16-=1
    v.s16+=lpCPU->mem->ReadByte(lpCPU->Code.op.u16)+1
    lpCPU->pc=v.u16
  end if
end sub

sub INS_BRK(lpCPU as CPU6510_T)
  lpCPU->pc+=1
  lpCPU->push(lpCPU->ph)
  lpCPU->push(lpCPU->pl)
  lpCPU->push(lpCPU->p )
  lpCPU->F.b=1
  lpCPU->F.i=1
  lpCPU->pc = lpCPU->mem->ReadUShort(&HFFFE)
end sub

sub INS_BVC(lpCPU as CPU6510_T)
  if lpCPU->F.v=0 then
    dim as MULTI v
    v.u16 =lpCPU->pc
    v.s16-=1
    v.s16+=lpCPU->mem->ReadByte(lpCPU->Code.op.u16)+1
    lpCPU->pc=v.u16
  end if
end sub

sub INS_BVS(lpCPU as CPU6510_T)
  if lpCPU->F.v then
    dim as MULTI v
    v.u16 =lpCPU->pc
    v.s16-=1
    v.s16+=lpCPU->mem->ReadByte(lpCPU->Code.op.u16)+1
    lpCPU->pc=v.u16
  end if
end sub

sub INS_CLC(lpCPU as CPU6510_T)
  lpCPU->F.C=0
end sub

sub INS_CLD(lpCPU as CPU6510_T)
  lpCPU->F.D=0
end sub

sub INS_CLI(lpCPU as CPU6510_T)
  lpCPU->F.I=0
end sub

sub INS_CLV(lpCPU as CPU6510_T)
  lpCPU->F.V=0
end sub

sub INS_CMP(lpCPU as CPU6510_T)
  dim as MULTI v
  v.u16 = lpCPU->A-lpCPU->mem->ReadUByte(lpCPU->Code.op.u16)
  lpCPU->F.c=iif(v.u16<=255,1,0)
  lpCPU->F.z=iif(v.ulo =  0,1,0)
  lpCPU->F.n=iif(v.slo <  0,1,0)
end sub

sub INS_CPX(lpCPU as CPU6510_T)
  dim as MULTI v
  v.u16 = lpCPU->X-lpCPU->mem->ReadUByte(lpCPU->Code.op.u16)
  lpCPU->F.c=iif(v.u16<=255,1,0)
  lpCPU->F.z=iif(v.ulo =  0,1,0)
  lpCPU->F.n=iif(v.slo <  0,1,0)
end sub

sub INS_CPY(lpCPU as CPU6510_T)
  dim as MULTI v
  v.u16 = lpCPU->Y-lpCPU->mem->ReadUByte(lpCPU->Code.op.u16)
  lpCPU->F.c=iif(v.u16<=255,1,0)
  lpCPU->F.z=iif(v.ulo =  0,1,0)
  lpCPU->F.n=iif(v.slo <  0,1,0)
end sub

sub INS_DEC(lpCPU as CPU6510_T)
  dim as MULTI v
  v.ulo=lpCPU->mem->ReadUByte(lpCPU->Code.op.u16)
  v.slo-=1
  lpCPU->F.z=iif(v.slo=0,1,0)
  lpCPU->F.n=iif(v.slo<0,1,0)
  lpCPU->mem->WriteUByte(lpCPU->Code.op.u16,v.ulo)
end sub

sub INS_DEX(lpCPU as CPU6510_T)
  lpCPU->sX-=1
  lpCPU->F.z=iif(lpCPU->X =0,1,0)
  lpCPU->F.n=iif(lpCPU->sX<0,1,0)
end sub

sub INS_DEY(lpCPU as CPU6510_T)
  lpCPU->sY-=1
  lpCPU->F.z=iif(lpCPU->Y =0,1,0)
  lpCPU->F.n=iif(lpCPU->sY<0,1,0)
end sub

sub INS_EOR(lpCPU as CPU6510_T)
  lpCPU->A=lpCPU->A xor lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  lpCPU->F.z=iif(lpCPU->A =0,1,0)
  lpCPU->F.n=iif(lpCPU->sA<0,1,0)
end sub

sub INS_INC(lpCPU as CPU6510_T)
  dim as MULTI v
  v.ulo=lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  v.s16+=1
  lpCPU->mem->WriteByte(lpCPU->Code.op.u16,v.ulo)
  lpCPU->F.z=iif(v.ulo=0,1,0)
  lpCPU->F.n=iif(v.slo<0,1,0)
end sub

sub INS_INX(lpCPU as CPU6510_T)
  dim as MULTI v
  v.ulo=lpCPU->X
  v.s16+=1
  lpCPU->X=v.ulo
  lpCPU->F.z=iif(v.ulo=0,1,0)
  lpCPU->F.n=iif(v.slo<0,1,0)
end sub

sub INS_INY(lpCPU as CPU6510_T)
  dim as MULTI v
  v.ulo=lpCPU->Y
  v.s16+=1
  lpCPU->Y=v.ulo
  lpCPU->F.z=iif(v.ulo=0,1,0)
  lpCPU->F.n=iif(v.slo<0,1,0)
end sub

sub INS_JMP(lpCPU as CPU6510_T)
  lpCPU->PC=lpCPU->Code.op.u16
end sub

sub INS_JSR(lpCPU as CPU6510_T)
  lpCPU->PC-=1
  lpCPU->Push(lpCPU->PH)
  lpCPU->Push(lpCPU->PL)
  lpCPU->PC=lpCPU->Code.op.u16
end sub

sub INS_LDA(lpCPU as CPU6510_T)
  lpCPU->A  =lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  lpCPU->F.Z=iif(lpCPU->A=0,1,0)
  lpCPU->F.N=iif(lpCPU->sA<0,1,0)
end sub

sub INS_LDX(lpCPU as CPU6510_T)
  lpCPU->X  =lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  lpCPU->F.Z=iif(lpCPU->X=0,1,0)
  lpCPU->F.N=iif(lpCPU->sX<0,1,0)
end sub

sub INS_LDY(lpCPU as CPU6510_T)
  lpCPU->Y  =lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  lpCPU->F.Z=iif(lpCPU->Y =0,1,0)
  lpCPU->F.N=iif(lpCPU->sY<0,1,0)
end sub

sub INS_LSR(lpCPU as CPU6510_T)
  dim as MULTI v
  v.ulo=lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  lpCPU->F.c=iif(v.ulo and &H01,1,0)
  v.ulo shr = 1
  lpCPU->mem->WriteUByte(lpCPU->Code.op.u16,v.ulo)
  lpCPU->F.z=iif(v.ulo=0,1,0)
  lpCPU->F.n=iif(v.slo<1,1,0)
end sub

sub INS_LSRA(lpCPU as CPU6510_T) ' ac
  lpCPU->F.c=iif(lpCPU->A and &H01,1,0)
  lpCPU->A shr = 1
  lpCPU->F.Z=iif(lpCPU->A =0,1,0)
  lpCPU->F.N=iif(lpCPU->sA<0,1,0)
end sub

sub INS_NOP(lpCPU as CPU6510_T)
  'dprint("NOP")
end sub

sub INS_ORA(lpCPU as CPU6510_T)
  lpCPU->A=lpCPU->A or lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  lpCPU->F.z=iif(lpCPU->A =0,1,0)
  lpCPU->F.n=iif(lpCPU->sA<0,1,0)
end sub

sub INS_PHA(lpCPU as CPU6510_T)
  lpCPU->Push(lpCPU->A)
end sub

sub INS_PHP(lpCPU as CPU6510_T)
  lpCPU->Push(lpCPU->P)
end sub

sub INS_PLA(lpCPU as CPU6510_T)
  lpCPU->A=lpCPU->Pull()
  lpCPU->F.z=iif(lpCPU->A =0,1,0)
  lpCPU->F.n=iif(lpCPU->sA<0,1,0)
end sub

sub INS_PLP(lpCPU as CPU6510_T)
  lpCPU->P=lpCPU->Pull()
end sub

sub INS_ROL(lpCPU as CPU6510_T)
  dim as MULTI v
  dim as ubyte cary
  v.ulo=lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  cary=iif(lpCPU->F.c=1,1,0)
  lpCPU->F.c=iif(v.ulo and &H80,1,0)
  v.ulo shl=1
  if cary then v.ulo or =1
  lpCPU->mem->WriteUByte(lpCPU->Code.op.u16,v.ulo)
  lpCPU->F.z=iif(v.ulo=0,1,0)
  lpCPU->F.n=iif(v.slo<1,1,0)
end sub
sub INS_ROLA(lpCPU as CPU6510_T) ' ac
  dim as ubyte cary
  cary=iif(lpCPU->F.c=1,1,0)
  lpCPU->F.c=iif(lpCPU->A and &H80,1,0)
  lpCPU->A shl= 1
  if cary then lpCPU->A or =1
  lpCPU->F.z=iif(lpCPU->A =0,1,0)
  lpCPU->F.n=iif(lpCPU->sA<0,1,0)
end sub

sub INS_ROR(lpCPU as CPU6510_T)
  dim as MULTI v
  dim as ubyte cary
  cary=iif(lpCPU->F.c=1,1,0)
  v.ulo=lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  lpCPU->F.c=iif(v.ulo and &H01,1,0)
  v.ulo shr=1
  if cary then v.ulo or = &H80
  lpCPU->mem->WriteUByte(lpCPU->Code.op.u16,v.ulo)
  lpCPU->F.z=iif(v.ulo=0,1,0)
  lpCPU->F.n=iif(v.slo<0,1,0)
end sub

sub INS_RORA(lpCPU as CPU6510_T) ' ac
  dim as ubyte cary
  cary=iif(lpCPU->F.c=1,1,0)
  lpCPU->F.c=iif(lpCPU->A and &H01,1,0)
  lpCPU->A shr= 1
  if cary then lpCPU->A or =&H80
  lpCPU->F.z=iif(lpCPU->A =0,1,0)
  lpCPU->F.n=iif(lpCPU->sA<0,1,0)
end sub

sub INS_RTI(lpCPU as CPU6510_T)
  lpCPU->P =lpCPU->pull()
  lpCPU->PL=lpCPU->pull()
  lpCPU->PH=lpCPU->pull()
  lpCPU->PC+=1
end sub

sub INS_RTS(lpCPU as CPU6510_T)
  lpCPU->PL=lpCPU->pull()
  lpCPU->PH=lpCPU->pull()
  lpCPU->PC+=1
end sub

sub INS_SBC(lpCPU as CPU6510_T)
  dim as multi v,b
  b.ulo=lpCPU->mem->ReadUbyte(lpCPU->Code.op.u16)
  v.u16=lpCPU->A - b.ulo
  if lpCPU->F.c=0 then v.s16-=1
  lpCPU->F.v=iif((((lpCPU->A xor b.ulo) and &H80) and _
  ((lpCPU->A xor v.ulo) and &H80)),1,0)
  lpCPU->A=v.ulo
  lpCPU->F.c=iif(v.u16<=255,1,0)
  lpCPU->F.z=iif(v.ulo =  0,1,0)
  lpCPU->F.n=iif(v.slo <  0,1,0)
end sub

sub INS_SEC(lpCPU as CPU6510_T)
  lpCPU->F.C=1
end sub

sub INS_SED(lpCPU as CPU6510_T)
  lpCPU->F.D=1
end sub

sub INS_SEI(lpCPU as CPU6510_T)
  lpCPU->F.I=1
end sub

sub INS_STA(lpCPU as CPU6510_T)
  lpCPU->mem->WriteUByte(lpCPU->code.op.u16,lpCPU->A)
end sub

sub INS_STX(lpCPU as CPU6510_T)
  lpCPU->mem->WriteUByte(lpCPU->code.op.u16,lpCPU->X)
end sub

sub INS_STY(lpCPU as CPU6510_T)
  lpCPU->mem->WriteUByte(lpCPU->code.op.u16,lpCPU->Y)
end sub

sub INS_TAX(lpCPU as CPU6510_T)
  lpCPU->X=lpCPU->A
  lpCPU->F.Z=iif(lpCPU->X =0,1,0)
  lpCPU->F.N=iif(lpCPU->sX<0,1,0)
end sub

sub INS_TAY(lpCPU as CPU6510_T)
  lpCPU->Y=lpCPU->A
  lpCPU->F.Z=iif(lpCPU->Y =0,1,0)
  lpCPU->F.N=iif(lpCPU->sY<0,1,0)
end sub

sub INS_TSX(lpCPU as CPU6510_T)
  lpCPU->X=lpCPU->S
  lpCPU->F.Z=iif(lpCPU->X =0,1,0)
  lpCPU->F.N=iif(lpCPU->sX<0,1,0)
end sub

sub INS_TXA(lpCPU as CPU6510_T)
  lpCPU->A=lpCPU->X
  lpCPU->F.Z=iif(lpCPU->A =0,1,0)
  lpCPU->F.N=iif(lpCPU->sA<0,1,0)
end sub

sub INS_TXS(lpCPU as CPU6510_T)
  lpCPU->S=lpCPU->X
end sub

sub INS_TYA(lpCPU as CPU6510_T)
  lpCPU->A=lpCPU->Y
  lpCPU->F.Z=iif(lpCPU->A =0,1,0)
  lpCPU->F.N=iif(lpCPU->sA<0,1,0)
end sub

INSTRUCTION_SET:
data   0,"BRK",_IMP   ,7,0,@INS_BRK
data   1,"ORA",_INDX  ,6,2,@INS_ORA
data   2,"***",_UNK   ,0,0,@INS_UNK
data   3,"***",_UNK   ,0,0,@INS_UNK
data   4,"***",_UNK   ,0,0,@INS_UNK
data   5,"ORA",_ZERO  ,3,2,@INS_ORA
data   6,"ASL",_ZERO  ,0,0,@INS_ASL
data   7,"***",_UNK   ,0,0,@INS_UNK
data   8,"PHP",_IMP   ,3,1,@INS_PHP
data   9,"ORA",_IMM   ,2,2,@INS_ORA
data  10,"ASL",_IMP   ,2,1,@INS_ASLA
data  11,"***",_UNK   ,0,0,@INS_UNK
data  12,"***",_UNK   ,0,0,@INS_UNK
data  13,"ORA",_ABS   ,4,3,@INS_ORA
data  14,"ASL",_ABS   ,0,0,@INS_ASL
data  15,"***",_UNK   ,0,0,@INS_UNK

data  16,"BPL",_REL   ,0,0,@INS_BPL
data  17,"ORA",_INDY  ,0,0,@INS_ORA
data  18,"***",_UNK   ,0,0,@INS_UNK
data  19,"***",_UNK   ,0,0,@INS_UNK
data  20,"***",_UNK   ,0,0,@INS_UNK
data  21,"ORA",_ZEROX ,0,0,@INS_ORA
data  22,"ASL",_ZEROX ,0,0,@INS_ASL
data  23,"***",_UNK   ,0,0,@INS_UNK
data  24,"CLC",_IMP   ,0,0,@INS_CLC
data  25,"ORA",_ABSY  ,0,0,@INS_ORA
data  26,"***",_UNK   ,0,0,@INS_UNK
data  27,"***",_UNK   ,0,0,@INS_UNK
data  28,"***",_UNK   ,0,0,@INS_UNK
data  29,"ORA",_ABSX  ,0,0,@INS_ORA
data  30,"ASL",_ABSX  ,0,0,@INS_ASL
data  31,"***",_UNK   ,0,0,@INS_UNK

data  32,"JSR",_ABS   ,0,0,@INS_JSR
data  33,"AND",_INDX  ,0,0,@INS_AND
data  34,"***",_UNK   ,0,0,@INS_UNK
data  35,"***",_UNK   ,0,0,@INS_UNK
data  36,"BIT",_ZERO  ,0,0,@INS_BIT
data  37,"AND",_ZERO  ,0,0,@INS_AND
data  38,"ROL",_ZERO  ,0,0,@INS_ROL
data  39,"***",_UNK   ,0,0,@INS_UNK
data  40,"PLP",_IMP   ,0,0,@INS_PLP
data  41,"AND",_IMM   ,0,0,@INS_AND
data  42,"ROL",_IMP   ,0,0,@INS_ROLA
data  43,"***",_UNK   ,0,0,@INS_UNK
data  44,"BIT",_ABS   ,0,0,@INS_BIT
data  45,"AND",_ABS   ,0,0,@INS_AND
data  46,"ROL",_ABS   ,0,0,@INS_ROL
data  47,"***",_UNK   ,0,0,@INS_UNK

data  48,"BMI",_REL   ,0,0,@INS_BMI
data  49,"AND",_INDY  ,0,0,@INS_AND
data  50,"***",_UNK   ,0,0,@INS_UNK
data  51,"***",_UNK   ,0,0,@INS_UNK
data  52,"***",_UNK   ,0,0,@INS_UNK
data  53,"AND",_ZEROX ,0,0,@INS_AND
data  54,"ROL",_ZEROX ,0,0,@INS_ROL
data  55,"***",_UNK   ,0,0,@INS_UNK
data  56,"SEC",_IMP   ,0,0,@INS_SEC
data  57,"AND",_ABSY  ,0,0,@INS_AND
data  58,"***",_UNK   ,0,0,@INS_UNK
data  59,"***",_UNK   ,0,0,@INS_UNK
data  60,"***",_UNK   ,0,0,@INS_UNK
data  61,"AND",_ABSX  ,0,0,@INS_AND
data  62,"ROL",_ABSX  ,0,0,@INS_ROL
data  63,"***",_UNK   ,0,0,@INS_UNK

data  64,"RTI",_IMP   ,0,0,@INS_RTI
data  65,"EOR",_INDX  ,0,0,@INS_EOR
data  66,"***",_UNK   ,0,0,@INS_UNK
data  67,"***",_UNK   ,0,0,@INS_UNK
data  68,"***",_UNK   ,0,0,@INS_UNK
data  69,"EOR",_ZERO  ,0,0,@INS_EOR
data  70,"LSR",_ZERO  ,0,0,@INS_LSR
data  71,"***",_UNK   ,0,0,@INS_UNK
data  72,"PHA",_IMP   ,0,0,@INS_PHA
data  73,"EOR",_IMM   ,0,0,@INS_EOR
data  74,"LSR",_IMP   ,0,0,@INS_LSRA
data  75,"***",_UNK   ,0,0,@INS_UNK
data  76,"JMP",_ABS   ,0,0,@INS_JMP
data  77,"EOR",_ABS   ,0,0,@INS_EOR
data  78,"LSR",_ABS   ,0,0,@INS_LSR
data  79,"***",_UNK   ,0,0,@INS_UNK

data  80,"BVC",_REL   ,0,0,@INS_BVC
data  81,"EOR",_INDY  ,0,0,@INS_EOR
data  82,"***",_UNK   ,0,0,@INS_UNK
data  83,"***",_UNK   ,0,0,@INS_UNK
data  84,"***",_UNK   ,0,0,@INS_UNK
data  85,"EOR",_ZEROX ,0,0,@INS_EOR
data  86,"LSR",_ZEROX ,0,0,@INS_LSR
data  87,"***",_UNK   ,0,0,@INS_UNK
data  88,"CLI",_IMP   ,0,0,@INS_CLI
data  89,"EOR",_ABSY  ,0,0,@INS_EOR
data  90,"***",_UNK   ,0,0,@INS_UNK
data  91,"***",_UNK   ,0,0,@INS_UNK
data  92,"***",_UNK   ,0,0,@INS_UNK
data  93,"EOR",_ABSX  ,0,0,@INS_EOR
data  94,"LSR",_ABSX  ,0,0,@INS_LSR
data  95,"***",_UNK   ,0,0,@INS_UNK

data  96,"RTS",_IMP   ,0,0,@INS_RTS
data  97,"ADC",_INDX  ,0,0,@INS_ADC
data  98,"***",_UNK   ,0,0,@INS_UNK
data  99,"***",_UNK   ,0,0,@INS_UNK
data 100,"***",_UNK   ,0,0,@INS_UNK
data 101,"ADC",_ZERO  ,0,0,@INS_ADC
data 102,"ROR",_ZERO  ,0,0,@INS_ROR
data 103,"***",_UNK   ,0,0,@INS_UNK
data 104,"PLA",_IMP   ,0,0,@INS_PLA
data 105,"ADC",_IMM   ,0,0,@INS_ADC
data 106,"ROR",_IMP   ,0,0,@INS_RORA
data 107,"***",_UNK   ,0,0,@INS_UNK
data 108,"JMP",_IND   ,0,0,@INS_JMP
data 109,"ADC",_ABS   ,0,0,@INS_ADC
data 110,"ROR",_ABS   ,0,0,@INS_ROR
data 111,"***",_UNK   ,0,0,@INS_UNK

data 112,"BVS",_REL   ,0,0,@INS_BVS
data 113,"ADC",_INDY  ,0,0,@INS_ADC
data 114,"***",_UNK   ,0,0,@INS_UNK
data 115,"***",_UNK   ,0,0,@INS_UNK
data 116,"***",_UNK   ,0,0,@INS_UNK
data 117,"ADC",_ZEROX ,0,0,@INS_ADC
data 118,"ROR",_ZEROX ,0,0,@INS_ROR
data 119,"***",_UNK   ,0,0,@INS_UNK
data 120,"SEI",_IMP   ,0,0,@INS_SEI
data 121,"ADC",_ABSY  ,0,0,@INS_ADC
data 122,"***",_UNK   ,0,0,@INS_UNK
data 123,"***",_UNK   ,0,0,@INS_UNK
data 124,"***",_UNK   ,0,0,@INS_UNK
data 125,"ADC",_ABSX  ,0,0,@INS_ADC
data 126,"ROR",_ABSX  ,0,0,@INS_ROR
data 127,"***",_UNK   ,0,0,@INS_UNK

data 128,"***",_UNK   ,0,0,@INS_UNK
data 129,"STA",_INDX  ,0,0,@INS_STA
data 130,"***",_UNK   ,0,0,@INS_UNK
data 131,"***",_UNK   ,0,0,@INS_UNK
data 132,"STY",_ZERO  ,0,0,@INS_STY
data 133,"STA",_ZERO  ,0,0,@INS_STA
data 134,"STX",_ZERO  ,0,0,@INs_STX
data 135,"***",_UNK   ,0,0,@INS_UNK
data 136,"DEY",_IMP   ,0,0,@INS_DEY
data 137,"***",_UNK   ,0,0,@INS_UNK
data 138,"TXA",_IMP   ,0,0,@INS_TXA
data 139,"***",_UNK   ,0,0,@INS_UNK
data 140,"STY",_ABS   ,0,0,@INS_STY
data 141,"STA",_ABS   ,0,0,@INS_STA
data 142,"STX",_ABS   ,0,0,@INS_STX
data 143,"***",_UNK   ,0,0,@INS_UNK

data 144,"BCC",_REL   ,0,0,@INS_BCC
data 145,"STA",_INDY  ,0,0,@INS_STA
data 146,"***",_UNK   ,0,0,@INS_UNK
data 147,"***",_UNK   ,0,0,@INS_UNK
data 148,"STY",_ZEROX ,0,0,@INS_STY
data 149,"STA",_ZEROX ,0,0,@INS_STA
data 150,"STX",_ZEROY ,0,0,@INS_STX
data 151,"***",_UNK   ,0,0,@INS_UNK
data 152,"TYA",_IMP   ,0,0,@INS_TYA
data 153,"STA",_ABSY  ,0,0,@INS_STA
data 154,"TXS",_IMP   ,0,0,@INS_TXS
data 155,"***",_UNK   ,0,0,@INS_UNK
data 156,"***",_UNK   ,0,0,@INS_UNK
data 157,"STA",_ABSX  ,0,0,@INS_STA
data 158,"***",_UNK   ,0,0,@INS_UNK
data 159,"***",_UNK   ,0,0,@INS_UNK

data 160,"LDY",_IMM   ,0,0,@INS_LDY
data 161,"LDA",_INDX  ,0,0,@INS_LDA
data 162,"LDX",_IMM   ,0,0,@INS_LDX
data 163,"***",_UNK   ,0,0,@INS_UNK
data 164,"LDY",_ZERO  ,0,0,@INS_LDY
data 165,"LDA",_ZERO  ,0,0,@INS_LDA
data 166,"LDX",_ZERO  ,0,0,@INS_LDX
data 167,"***",_UNK   ,0,0,@INS_UNK
data 168,"TAY",_IMP   ,0,0,@INS_TAY
data 169,"LDA",_IMM   ,0,0,@INS_LDA
data 170,"TAX",_IMP   ,0,0,@INS_TAX
data 171,"***",_UNK   ,0,0,@INS_UNK
data 172,"LDY",_ABS   ,0,0,@INS_LDY
data 173,"LDA",_ABS   ,0,0,@INS_LDA
data 174,"LDX",_ABS   ,0,0,@INS_LDX
data 175,"***",_UNK   ,0,0,@INS_UNK

data 176,"BCS",_REL   ,0,0,@INS_BCS
data 177,"LDA",_INDY  ,0,0,@INS_LDA
data 178,"***",_UNK   ,0,0,@INS_UNK
data 179,"***",_UNK   ,0,0,@INS_UNK
data 180,"LDY",_ZEROX ,0,0,@INS_LDY
data 181,"LDA",_ZEROX ,0,0,@INS_LDA
data 182,"LDX",_ZEROY ,0,0,@INS_LDX
data 183,"***",_UNK   ,0,0,@INS_UNK
data 184,"CLV",_IMP   ,0,0,@INS_CLV
data 185,"LDA",_ABSY  ,0,0,@INS_LDA
data 186,"TSX",_IMP   ,0,0,@INS_TSX
data 187,"***",_UNK   ,0,0,@INS_UNK
data 188,"LDY",_ABSX  ,0,0,@INS_LDY
data 189,"LDA",_ABSX  ,0,0,@INS_LDA
data 190,"LDX",_ABSY  ,0,0,@INS_LDX
data 191,"***",_UNK   ,0,0,@INS_UNK

data 192,"CPY",_IMM   ,0,0,@INS_CPY
data 193,"CMP",_INDX  ,0,0,@INS_CMP
data 194,"***",_UNK   ,0,0,@INS_UNK
data 195,"***",_UNK   ,0,0,@INS_UNK
data 196,"CPY",_ZERO  ,0,0,@INS_CPY
data 197,"CMP",_ZERO  ,0,0,@INS_CMP
data 198,"DEC",_ZERO  ,0,0,@INS_DEC
data 199,"***",_UNK   ,0,0,@INS_UNK
data 200,"INY",_IMP   ,0,0,@INS_INY
data 201,"CMP",_IMM   ,0,0,@INS_CMP
data 202,"DEX",_IMP   ,0,0,@INS_DEX
data 203,"***",_UNK   ,0,0,@INS_UNK
data 204,"CPY",_ABS   ,0,0,@INS_CPY
data 205,"CMP",_ABS   ,0,0,@INS_CMP
data 206,"DEC",_ABS   ,0,0,@INS_DEC
data 207,"***",_UNK   ,0,0,@INS_UNK

data 208,"BNE",_REL   ,0,0,@INS_BNE
data 209,"CMP",_INDY  ,0,0,@INS_CMP
data 210,"***",_UNK   ,0,0,@INS_UNK
data 211,"***",_UNK   ,0,0,@INS_UNK
data 212,"***",_UNK   ,0,0,@INS_UNK
data 213,"CMP",_ZEROX ,0,0,@INS_CMP
data 214,"DEC",_ZEROX ,0,0,@INS_DEC
data 215,"***",_UNK   ,0,0,@INS_UNK
data 216,"CLD",_IMP   ,0,0,@INS_CLD
data 217,"CMP",_ABSY  ,0,0,@INS_CMP
data 218,"***",_UNK   ,0,0,@INS_UNK
data 219,"***",_UNK   ,0,0,@INS_UNK
data 220,"***",_UNK   ,0,0,@INS_UNK
data 221,"CMP",_ABSX  ,0,0,@INS_CMP
data 222,"DEC",_ABSX  ,0,0,@INS_DEC
data 223,"***",_UNK   ,0,0,@INS_UNK

data 224,"CPX",_IMM   ,0,0,@INS_CPX
data 225,"SBC",_INDX  ,0,0,@INS_SBC
data 226,"***",_UNK   ,0,0,@INS_UNK
data 227,"***",_UNK   ,0,0,@INS_UNK
data 228,"CPX",_ZERO  ,0,0,@INS_CPX
data 229,"SBC",_ZERO  ,0,0,@INS_SBC
data 230,"INC",_ZERO  ,0,0,@INS_INC
data 231,"***",_UNK   ,0,0,@INS_UNK
data 232,"INX",_IMP   ,0,0,@INS_INX
data 233,"SBC",_IMM   ,0,0,@INS_SBC
data 234,"NOP",_IMP   ,0,0,@INS_NOP
data 235,"***",_UNK   ,0,0,@INS_UNK
data 236,"CPX",_ABS   ,0,0,@INS_CPX
data 237,"SBC",_ABS   ,0,0,@INS_SBC
data 238,"INC",_ABS   ,0,0,@INS_INC
data 239,"***",_UNK   ,0,0,@INS_UNK

data 240,"BEQ",_REL   ,0,0,@INS_BEQ
data 241,"SBC",_INDY  ,0,0,@INS_SBC
data 242,"***",_UNK   ,0,0,@INS_UNK
data 243,"***",_UNK   ,0,0,@INS_UNK
data 244,"***",_UNK   ,0,0,@INS_UNK
data 245,"SBC",_ZEROX ,0,0,@INS_SBC
data 246,"INC",_ZEROX ,0,0,@INS_INC
data 247,"***",_UNK   ,0,0,@INS_UNK
data 248,"SED",_IMP   ,0,0,@INS_SED
data 249,"SBC",_ABSY  ,0,0,@INS_SBC
data 250,"***",_UNK   ,0,0,@INS_UNK
data 251,"***",_UNK   ,0,0,@INS_UNK
data 252,"***",_UNK   ,0,0,@INS_UNK
data 253,"SBC",_ABSX  ,0,0,@INS_SBC
data 254,"INC",_ABSX  ,0,0,@INS_INC
data 255,"***",_UNK   ,0,0,@INS_UNK

ADDRESS_MODES:
data "UNK"
data "IMP"
data "IMM"
data "ABS"
data "ZERO"
data "ZEROX"
data "ZEROY"
data "ABSX"
data "ABSY"
data "REL"
data "INDX"
data "INDY"
data "IND"



KERNAL_ROM:
data 133, 86, 32, 15,188,165, 97,201,136,144,  3, 32,212,186, 32,204
data 188,165,  7, 24,105,129,240,243, 56,233,  1, 72,162,  5,181,105
data 180, 97,149, 97,148,105,202, 16,245,165, 86,133,112, 32, 83,184
data  32,180,191,169,196,160,191, 32, 89,224,169,  0,133,111,104, 32
data 185,186, 96,133,113,132,114, 32,202,187,169, 87, 32, 40,186, 32
data  93,224,169, 87,160,  0, 76, 40,186,133,113,132,114, 32,199,187
data 177,113,133,103,164,113,200,152,208,  2,230,114,133,113,164,114
data  32, 40,186,165,113,164,114, 24,105,  5,144,  1,200,133,113,132
data 114, 32,103,184,169, 92,160,  0,198,103,208,228, 96,152, 53, 68
data 122,  0,104, 40,177, 70,  0, 32, 43,188, 48, 55,208, 32, 32,243
data 255,134, 34,132, 35,160,  4,177, 34,133, 98,200,177, 34,133,100
data 160,  8,177, 34,133, 99,200,177, 34,133,101, 76,227,224,169,139
data 160,  0, 32,162,187,169,141,160,224, 32, 40,186,169,146,160,224
data  32,103,184,166,101,165, 98,133,101,134, 98,166, 99,165,100,133
data  99,134,100,169,  0,133,102,165, 97,133,112,169,128,133, 97, 32
data 215,184,162,139,160,  0, 76,212,187,201,240,208,  7,132, 56,134
data  55, 76, 99,166,170,208,  2,162, 30, 76, 55,164, 32,210,255,176
data 232, 96, 32,207,255,176,226, 96, 32,173,228,176,220, 96, 32,198
data 255,176,214, 96, 32,228,255,176,208, 96, 32,138,173, 32,247,183
data 169,225, 72,169, 70, 72,173, 15,  3, 72,173, 12,  3,174, 13,  3
data 172, 14,  3, 40,108, 20,  0,  8,141, 12,  3,142, 13,  3,140, 14
data   3,104,141,15,3,96,32,212,225,166,45,164,46,169,43,32
data 216,255,176,149,96,169,1,44,169,0,133,10,32,212,225,165
data  10,166,43,164,44,32,213,255,176,87,165,10,240,23,162,28
data  32,183,255,41,16,208,23,165,122,201,2,240,7,169,100,160
data 163,76,30,171,96,32,183,255,41,191,240,5,162,29,76,55
data 164,165,123,201,2,208,14,134,45,132,46,169,118,160,163,32
data  30,171,76,42,165,32,142,166,32,51,165,76,119,166,32,25
data 226,32,192,255,176,11,96,32,25,226,165,73,32,195,255,144
data 195,76,249,224,169,0,32,189,255,162,1,160,0,32,186,255
data  32,6,226,32,87,226,32,6,226,32,0,226,160,0,134,73
data  32,186,255,32,6,226,32,0,226,138,168,166,73,76,186,255
data  32,14,226,76,158,183,32,121,0,208,2,104,104,96,32,253
data 174,32,121,0,208,247,76,8,175,169,0,32,189,255,32,17
data 226,32,158,183,134,73,138,162,1,160,0,32,186,255,32,6
data 226,32,0,226,134,74,160,0,165,73,224,3,144,1,136,32
data 186,255,32,6,226,32,0,226,138,168,166,74,165,73,32,186
data 255,32,6,226,32,14,226,32,158,173,32,163,182,166,34,164
data  35,76,189,255,169,224,160,226,32,103,184,32,12,188,169,229
data 160,226,166,110,32,7,187,32,12,188,32,204,188,169,0,133
data 111,32,83,184,169,234,160,226,32,80,184,165,102,72,16,13
data  32,73,184,165,102,48,9,165,18,73,255,133,18,32,180,191
data 169,234,160,226,32,103,184,104,16,3,32,180,191,169,239,160
data 226,76,67,224,32,202,187,169,0,133,18,32,107,226,162,78
data 160,0,32,246,224,169,87,160,0,32,162,187,169,0,133,102
data 165,18,32,220,226,169,78,160,0,76,15,187,72,76,157,226
data 129,73,15,218,162,131,73,15,218,162,127,0,0,0,0,5
data 132,230,26,45,27,134,40,7,251,248,135,153,104,137,1,135
data  35,53,223,225,134,165,93,231,40,131,73,15,218,162,165,102
data  72,16,3,32,180,191,165,97,72,201,129,144,7,169,188,160
data 185,32,15,187,169,62,160,227,32,67,224,104,201,129,144,7
data 169,224,160,226,32,80,184,104,16,3,76,180,191,96,11,118
data 179,131,189,211,121,30,244,166,245,123,131,252,176,16,124,12
data  31,103,202,124,222,83,203,193,125,20,100,112,76,125,183,234
data  81,122,125,99,48,136,126,126,146,68,153,58,126,76,204,145
data 199,127,170,170,170,19,129,0,0,0,0,32,204,255,169,0
data 133,19,32,122,166,88,162,128,108,0,3,138,48,3,76,58
data 164,76,116,164,32,83,228,32,191,227,32,34,228,162,251,154
data 208,228,230,122,208,2,230,123,173,96,234,201,58,176,10,201
data  32,240,239,56,233,48,56,233,208,96,128,79,199,82,88,169
data  76,133,84,141,16,3,169,72,160,178,141,17,3,140,18,3
data 169,145,160,179,133,5,132,6,169,170,160,177,133,3,132,4
data 162,28,189,162,227,149,115,202,16,248,169,3,133,83,169,0
data 133,104,133,19,133,24,162,1,142,253,1,142,252,1,162,25
data 134,22,56,32,156,255,134,43,132,44,56,32,153,255,134,55
data 132,56,134,51,132,52,160,0,152,145,43,230,43,208,2,230
data  44,96,165,43,164,44,32,8,164,169,115,160,228,32,30,171
data 165,55,56,229,43,170,165,56,229,44,32,205,189,169,96,160
data 228,32,30,171,76,68,166,139,227,131,164,124,165,26,167,228
data 167,134,174,162,11,189,71,228,157,0,3,202,16,247,96,0
data  32,66,65,83,73,67,32,66,89,84,69,83,32,70,82,69
data  69,13,0,147,13,32,32,32,32,42,42,42,42,32,67,79
data  77,77,79,68,79,82,69,32,54,52,32,66,65,83,73,67
data  32,86,50,32,42,42,42,42,13,13,32,54,52,75,32,82
data  65,77,32,83,89,83,84,69,77,32,32,0,92,72,32,201
data 255,170,104,144,1,138,96,170,170,170,170,170,170,170,170,170
data 170,170,170,170,170,170,170,170,170,170,170,170,170,170,170,170
data 170,170,170,170,170,170,170,170,170,170,173,134,2,145,243,96
data 105,2,164,145,200,208,4,197,161,208,247,96,25,38,68,25
data  26,17,232,13,112,12,6,6,209,2,55,1,174,0,105,0
data 162,0,160,220,96,162,40,160,25,96,176,7,134,214,132,211
data  32,108,229,166,214,164,211,96,32,160,229,169,0,141,145,2
data 133,207,169,72,141,143,2,169,235,141,144,2,169,10,141,137
data   2,141,140,2,169,14,141,134,2,169,4,141,139,2,169,12
data 133,205,133,204,173,136,2,9,128,168,169,0,170,148,217,24
data 105,40,144,1,200,232,224,26,208,243,169,255,149,217,162,24
data  32,255,233,202,16,250,160,0,132,211,132,214,166,214,165,211
data 180,217,48,8,24,105,40,133,211,202,16,244,181,217,41,3
data  13,136,2,133,210,189,240,236,133,209,169,39,232,180,217,48
data   6,24,105,40,232,16,246,133,213,96,32,160,229,76,102,229
data 169,3,133,154,169,0,133,153,162,47,189,184,236,157,255,207
data 202,208,247,96,172,119,2,162,0,189,120,2,157,119,2,232
data 228,198,208,245,198,198,152,88,24,96,32,22,231,165,198,133
data 204,141,146,2,240,247,120,165,207,240,12,165,206,174,135,2
data 160,0,132,207,32,19,234,32,180,229,201,131,208,16,162,9
data 120,134,198,189,230,236,157,118,2,202,208,247,240,207,201,13
data 208,200,164,213,132,208,177,209,201,32,208,3,136,208,247,200
data 132,200,160,0,140,146,2,132,211,132,212,165,201,48,27,166
data 214,32,237,230,228,201,208,18,165,202,133,211,197,200,144,10
data 176,43,152,72,138,72,165,208,240,147,164,211,177,209,133,215
data  41,63,6,215,36,215,16,2,9,128,144,4,166,212,208,4
data 112,2,9,64,230,211,32,132,230,196,200,208,23,169,0,133
data 208,169,13,166,153,224,3,240,6,166,154,224,3,240,3,32
data  22,231,169,13,133,215,104,170,104,168,165,215,201,222,208,2
data 169,255,24,96,201,34,208,8,165,212,73,1,133,212,169,34
data  96,9,64,166,199,240,2,9,128,166,216,240,2,198,216,174
data 134,2,32,19,234,32,182,230,104,168,165,216,240,2,70,212
data 104,170,104,24,88,96,32,179,232,230,211,165,213,197,211,176
data  63,201,79,240,50,173,146,2,240,3,76,103,233,166,214,224
data  25,144,7,32,234,232,198,214,166,214,22,217,86,217,232,181
data 217,9,128,149,217,202,165,213,24,105,40,133,213,181,217,48
data   3,202,208,249,76,240,233,198,214,32,124,232,169,0,133,211
data  96,166,214,208,6,134,211,104,104,208,157,202,134,214,32,108
data 229,164,213,132,211,96,72,133,215,138,72,152,72,169,0,133
data 208,164,211,165,215,16,3,76,212,231,201,13,208,3,76,145
data 232,201,32,144,16,201,96,144,4,41,223,208,2,41,63,32
data 132,230,76,147,230,166,216,240,3,76,151,230,201,20,208,46
data 152,208,6,32,1,231,76,115,231,32,161,232,136,132,211,32
data  36,234,200,177,209,136,145,209,200,177,243,136,145,243,200,196
data 213,208,239,169,32,145,209,173,134,2,145,243,16,77,166,212
data 240,3,76,151,230,201,18,208,2,133,199,201,19,208,3,32
data 102,229,201,29,208,23,200,32,179,232,132,211,136,196,213,144
data   9,198,214,32,124,232,160,0,132,211,76,168,230,201,17,208
data  29,24,152,105,40,168,230,214,197,213,144,236,240,234,198,214
data 233,40,144,4,133,211,208,248,32,124,232,76,168,230,32,203
data 232,76,68,236,41,127,201,127,208,2,169,94,201,32,144,3
data  76,145,230,201,13,208,3,76,145,232,166,212,208,63,201,20
data 208,55,164,213,177,209,201,32,208,4,196,211,208,7,192,79
data 240,36,32,101,233,164,213,32,36,234,136,177,209,200,145,209
data 136,177,243,200,145,243,136,196,211,208,239,169,32,145,209,173
data 134,2,145,243,230,216,76,168,230,166,216,240,5,9,64,76
data 151,230,201,17,208,22,166,214,240,55,198,214,165,211,56,233
data  40,144,4,133,211,16,42,32,108,229,208,37,201,18,208,4
data 169,0,133,199,201,29,208,18,152,240,9,32,161,232,136,132
data 211,76,168,230,32,1,231,76,168,230,201,19,208,6,32,68
data 229,76,168,230,9,128,32,203,232,76,79,236,70,201,166,214
data 232,224,25,208,3,32,234,232,181,217,16,244,134,214,76,108
data 229,162,0,134,216,134,199,134,212,134,211,32,124,232,76,168
data 230,162,2,169,0,197,211,240,7,24,105,40,202,208,246,96
data 198,214,96,162,2,169,39,197,211,240,7,24,105,40,202,208
data 246,96,166,214,224,25,240,2,230,214,96,162,15,221,218,232
data 240,4,202,16,248,96,142,134,2,96,144,5,28,159,156,30
data  31,158,129,149,150,151,152,153,154,155,165,172,72,165,173,72
data 165,174,72,165,175,72,162,255,198,214,198,201,206,165,2,232
data  32,240,233,224,24,176,12,189,241,236,133,172,181,218,32,200
data 233,48,236,32,255,233,162,0,181,217,41,127,180,218,16,2
data   9,128,149,217,232,224,24,208,239,165,241,9,128,133,241,165
data 217,16,195,230,214,238,165,2,169,127,141,0,220,173,1,220
data 201,251,8,169,127,141,0,220,40,208,11,160,0,234,202,208
data 252,136,208,249,132,198,166,214,104,133,175,104,133,174,104,133
data 173,104,133,172,96,166,214,232,181,217,16,251,142,165,2,224
data  24,240,14,144,12,32,234,232,174,165,2,202,198,214,76,218
data 230,165,172,72,165,173,72,165,174,72,165,175,72,162,25,202
data  32,240,233,236,165,2,144,14,240,12,189,239,236,133,172,181
data 216,32,200,233,48,233,32,255,233,162,23,236,165,2,144,15
data 181,218,41,127,180,217,16,2,9,128,149,218,202,208,236,174
data 165,2,32,218,230,76,88,233,41,3,13,136,2,133,173,32
data 224,233,160,39,177,172,145,209,177,174,145,243,136,16,245,96
data  32,36,234,165,172,133,174,165,173,41,3,9,216,133,175,96
data 189,240,236,133,209,181,217,41,3,13,136,2,133,210,96,160
data  39,32,240,233,32,36,234,169,32,145,209,32,218,228,234,136
data  16,245,96,168,169,2,133,205,32,36,234,152,164,211,145,209
data 138,145,243,96,165,209,133,243,165,210,41,3,9,216,133,244
data  96,32,234,255,165,204,208,41,198,205,208,37,169,20,133,205
data 164,211,70,207,174,135,2,177,209,176,17,230,207,133,206,32
data  36,234,177,243,141,135,2,174,134,2,165,206,73,128,32,28
data 234,165,1,41,16,240,10,160,0,132,192,165,1,9,32,208
data   8,165,192,208,6,165,1,41,31,133,1,32,135,234,173,13
data 220,104,168,104,170,104,64,169,0,141,141,2,160,64,132,203
data 141,0,220,174,1,220,224,255,240,97,168,169,129,133,245,169
data 235,133,246,169,254,141,0,220,162,8,72,173,1,220,205,1
data 220,208,248,74,176,22,72,177,245,201,5,176,12,201,3,240
data   8,13,141,2,141,141,2,16,2,132,203,104,200,192,65,176
data  11,202,208,223,56,104,42,141,0,220,208,204,104,108,143,2
data 164,203,177,245,170,196,197,240,7,160,16,140,140,2,208,54
data  41,127,44,138,2,48,22,112,73,201,127,240,41,201,20,240
data  12,201,32,240,8,201,29,240,4,201,17,208,53,172,140,2
data 240,5,206,140,2,208,43,206,139,2,208,38,160,4,140,139
data   2,164,198,136,16,28,164,203,132,197,172,141,2,140,142,2
data 224,255,240,14,138,166,198,236,137,2,176,6,157,119,2,232
data 134,198,169,127,141,0,220,96,173,141,2,201,3,208,21,205
data 142,2,240,238,173,145,2,48,29,173,24,208,73,2,141,24
data 208,76,118,235,10,201,8,144,2,169,6,170,189,121,235,133
data 245,189,122,235,133,246,76,224,234,129,235,194,235,3,236,120
data 236,20,13,29,136,133,134,135,17,51,87,65,52,90,83,69
data   1,53,82,68,54,67,70,84,88,55,89,71,56,66,72,85
data  86,57,73,74,48,77,75,79,78,43,80,76,45,46,58,64
data  44,92,42,59,19,1,61,94,47,49,95,4,50,32,2,81
data   3,255,148,141,157,140,137,138,139,145,35,215,193,36,218,211
data 197,1,37,210,196,38,195,198,212,216,39,217,199,40,194,200
data 213,214,41,201,202,48,205,203,207,206,219,208,204,221,62,91
data 186,60,169,192,93,147,1,61,222,63,33,95,4,34,160,2
data 209,131,255,148,141,157,140,137,138,139,145,150,179,176,151,173
data 174,177,1,152,178,172,153,188,187,163,189,154,183,165,155,191
data 180,184,190,41,162,181,48,167,161,185,170,166,175,182,220,62
data  91,164,60,168,223,93,147,1,61,222,63,129,95,4,149,160
data   2,171,131,255,201,14,208,7,173,24,208,9,2,208,9,201
data 142,208,11,173,24,208,41,253,141,24,208,76,168,230,201,8
data 208,7,169,128,13,145,2,48,9,201,9,208,238,169,127,45
data 145,2,141,145,2,76,168,230,255,255,255,255,255,255,255,255
data  28,23,1,159,26,19,5,255,156,18,4,30,3,6,20,24
data  31,25,7,158,2,8,21,22,18,9,10,146,13,11,15,14
data 255,16,12,255,255,27,0,255,28,255,29,255,255,31,30,255
data 144,6,255,5,255,255,17,255,255,0,0,0,0,0,0,0
data   0,0,0,0,0,0,0,0,0,0,155,55,0,0,0,8
data   0,20,15,0,0,0,0,0,0,14,6,1,2,3,4,0
data   1,2,3,4,5,6,7,76,79,65,68,13,82,85,78,13
data   0, 40, 80,120,160,200,240, 24, 64,104,144,184,224, 8, 48, 88
data 128,168,208,248, 32, 72,112,152,192,  9, 64, 44, 9, 32, 32,164
data 240, 72, 36,148, 16, 10, 56,102,163, 32, 64,237,70,148, 70,163
data 104,133,149,120,32,151,238,201,63,208,3,32,133,238,173,0
data 221,9,8,141,0,221,120,32,142,238,32,151,238,32,179,238
data 120,32,151,238,32,169,238,176,100,32,133,238,36,163,16,10
data  32,169,238,144,251,32,169,238,176,251,32,169,238,144,251,32
data 142,238,169,8,133,165,173,0,221,205,0,221,208,248,10,144
data  63,102,149,176,5,32,160,238,208,3,32,151,238,32,133,238
data 234,234,234,234,173,0,221,41,223,9,16,141,0,221,198,165
data 208,212,169,4,141,7,220,169,25,141,15,220,173,13,220,173
data  13,220,41,2,208,10,32,169,238,176,244,88,96,169,128,44
data 169,3,32,28,254,88,24,144,74,133,149,32,54,237,173,0
data 221,41,247,141,0,221,96,133,149,32,54,237,120,32,160,238
data  32,190,237,32,133,238,32,169,238,48,251,88,96,36,148,48
data   5,56,102,148,208,5,72,32,64,237,104,133,149,24,96,120
data  32,142,238,173,0,221,9,8,141,0,221,169,95,44,169,63
data  32,17,237,32,190,237,138,162,10,202,208,253,170,32,133,238
data  76,151,238,120,169,0,133,165,32,133,238,32,169,238,16,251
data 169,1,141,7,220,169,25,141,15,220,32,151,238,173,13,220
data 173,13,220,41,2,208,7,32,169,238,48,244,16,24,165,165
data 240,5,169,2,76,178,237,32,160,238,32,133,238,169,64,32
data  28,254,230,165,208,202,169,8,133,165,173,0,221,205,0,221
data 208,248,10,16,245,102,164,173,0,221,205,0,221,208,248,10
data  48,245,198,165,208,228,32,160,238,36,144,80,3,32,6,238
data 165,164,88,24,96,173,0,221,41,239,141,0,221,96,173,0
data 221,9,16,141,0,221,96,173,0,221,41,223,141,0,221,96
data 173,0,221,9,32,141,0,221,96,173,0,221,205,0,221,208
data 248,10,96,138,162,184,202,208,253,170,96,165,180,240,71,48
data  63,70,182,162,0,144,1,202,138,69,189,133,189,198,180,240
data   6,138,41,4,133,181,96,169,32,44,148,2,240,20,48,28
data 112,20,165,189,208,1,202,198,180,173,147,2,16,227,198,180
data 208,223,230,180,208,240,165,189,240,237,208,234,112,233,80,230
data 230,180,162,255,208,203,173,148,2,74,144,7,44,1,221,16
data  29,80,30,169,0,133,189,133,181,174,152,2,134,180,172,157
data   2,204,158,2,240,19,177,249,133,182,238,157,2,96,169,64
data  44,169,16,13,151,2,141,151,2,169,1,141,13,221,77,161
data   2,9,128,141,161,2,141,13,221,96,162,9,169,32,44,147
data   2,240,1,202,80,2,202,202,96,166,169,208,51,198,168,240
data  54,48,13,165,167,69,171,133,171,70,167,102,170,96,198,168
data 165,167,240,103,173,147,2,10,169,1,101,168,208,239,169,144
data 141,13,221,13,161,2,141,161,2,133,169,169,2,76,59,239
data 165,167,208,234,133,169,96,172,155,2,200,204,156,2,240,42
data 140,155,2,136,165,170,174,152,2,224,9,240,4,74,232,208
data 248,145,247,169,32,44,148,2,240,180,48,177,165,167,69,171
data 240,3,112,169,44,80,166,169,1,44,169,4,44,169,128,44
data 169,2,13,151,2,141,151,2,76,126,239,165,170,208,241,240
data 236,133,154,173,148,2,74,144,41,169,2,44,1,221,16,29
data 208,32,173,161,2,41,2,208,249,44,1,221,112,251,173,1
data 221,9,2,141,1,221,44,1,221,112,7,48,249,169,64,141
data 151,2,24,96,32,40,240,172,158,2,200,204,157,2,240,244
data 140,158,2,136,165,158,145,249,173,161,2,74,176,30,169,16
data 141,14,221,173,153,2,141,4,221,173,154,2,141,5,221,169
data 129,32,59,239,32,6,239,169,17,141,14,221,96,133,153,173
data 148,2,74,144,40,41,8,240,36,169,2,44,1,221,16,173
data 240,34,173,161,2,74,176,250,173,1,221,41,253,141,1,221
data 173,1,221,41,4,240,249,169,144,24,76,59,239,173,161,2
data  41,18,240,243,24,96,173,151,2,172,156,2,204,155,2,240
data 11,41,247,141,151,2,177,247,238,156,2,96,9,8,141,151
data 2,169,0,96,72,173,161,2,240,17,173,161,2,41,3,208
data 249,169,16,141,13,221,169,0,141,161,2,104,96,13,73,47
data 79,32,69,82,82,79,82,32,163,13,83,69,65,82,67,72
data 73,78,71,160,70,79,82,160,13,80,82,69,83,83,32,80
data 76,65,89,32,79,78,32,84,65,80,197,80,82,69,83,83
data 32,82,69,67,79,82,68,32,38,32,80,76,65,89,32,79
data 78,32,84,65,80,197,13,76,79,65,68,73,78,199,13,83
data 65,86,73,78,71,160,13,86,69,82,73,70,89,73,78,199
data 13,70,79,85,78,68,160,13,79,75,141,36,157,16,13,185
data 189,240,8,41,127,32,210,255,200,40,16,243,24,96,165,153
data 208,8,165,198,240,15,120,76,180,229,201,2,208,24,132,151
data 32,134,240,164,151,24,96,165,153,208,11,165,211,133,202,165
data 214,133,201,76,50,230,201,3,208,9,133,208,165,213,133,200
data 76,50,230,176,56,201,2,240,63,134,151,32,153,241,176,22
data 72,32,153,241,176,13,208,5,169,64,32,28,254,198,166,166
data 151,104,96,170,104,138,166,151,96,32,13,248,208,11,32,65
data 248,176,17,169,0,133,166,240,240,177,178,24,96,165,144,240
data 4,169,13,24,96,76,19,238,32,78,241,176,247,201,0,208
data 242,173,151,2,41,96,208,233,240,238,72,165,154,201,3,208
data 4,104,76,22,231,144,4,104,76,221,237,74,104,133,158,138
data 72,152,72,144,35,32,13,248,208,14,32,100,248,176,14,169
data 2,160,0,145,178,200,132,166,165,158,145,178,24,104,168,104
data 170,165,158,144,2,169,0,96,32,23,240,76,252,241,32,15
data 243,240,3,76,1,247,32,31,243,165,186,240,22,201,3,240
data 18,176,20,201,2,208,3,76,77,240,166,185,224,96,240,3
data 76,10,247,133,153,24,96,170,32,9,237,165,185,16,6,32
data 204,237,76,72,242,32,199,237,138,36,144,16,230,76,7,247
data 32,15,243,240,3,76,1,247,32,31,243,165,186,208,3,76
data 13,247,201,3,240,15,176,17,201,2,208,3,76,225,239,166
data 185,224,96,240,234,133,154,24,96,170,32,12,237,165,185,16
data 5,32,190,237,208,3,32,185,237,138,36,144,16,231,76,7
data 247,32,20,243,240,2,24,96,32,31,243,138,72,165,186,240
data 80,201,3,240,76,176,71,201,2,208,29,104,32,242,242,32
data 131,244,32,39,254,165,248,240,1,200,165,250,240,1,200,169
data 0,133,248,133,250,76,125,244,165,185,41,15,240,35,32,208
data 247,169,0,56,32,221,241,32,100,248,144,4,104,169,0,96
data 165,185,201,98,208,11,169,5,32,106,247,76,241,242,32,66
data 246,104,170,198,152,228,152,240,20,164,152,185,89,2,157,89
data 2,185,99,2,157,99,2,185,109,2,157,109,2,24,96,169
data 0,133,144,138,166,152,202,48,21,221,89,2,208,248,96,189
data 89,2,133,184,189,99,2,133,186,189,109,2,133,185,96,169
data 0,133,152,162,3,228,154,176,3,32,254,237,228,153,176,3
data 32,239,237,134,154,169,0,133,153,96,166,184,208,3,76,10
data 247,32,15,243,208,3,76,254,246,166,152,224,10,144,3,76
data 251,246,230,152,165,184,157,89,2,165,185,9,96,133,185,157
data 109,2,165,186,157,99,2,240,90,201,3,240,86,144,5,32
data 213,243,144,79,201,2,208,3,76,9,244,32,208,247,176,3
data 76,19,247,165,185,41,15,208,31,32,23,248,176,54,32,175
data 245,165,183,240,10,32,234,247,144,24,240,40,76,4,247,32
data 44,247,240,32,144,12,176,244,32,56,248,176,23,169,4,32
data 106,247,169,191,164,185,192,96,240,7,160,0,169,2,145,178
data 152,133,166,24,96,165,185,48,250,164,183,240,246,169,0,133
data 144,165,186,32,12,237,165,185,9,240,32,185,237,165,144,16
data 5,104,104,76,7,247,165,183,240,12,160,0,177,187,32,221
data 237,200,196,183,208,246,76,84,246,32,131,244,140,151,2,196
data 183,240,10,177,187,153,147,2,200,192,4,208,242,32,74,239
data 142,152,2,173,147,2,41,15,240,28,10,170,173,166,2,208
data 9,188,193,254,189,192,254,76,64,244,188,235,228,189,234,228
data 140,150,2,141,149,2,173,149,2,10,32,46,255,173,148,2
data 74,144,9,173,1,221,10,176,3,32,13,240,173,155,2,141
data 156,2,173,158,2,141,157,2,32,39,254,165,248,208,5,136
data 132,248,134,247,165,250,208,5,136,132,250,134,249,56,169,240
data 76,45,254,169,127,141,13,221,169,6,141,3,221,141,1,221
data 169,4,13,0,221,141,0,221,160,0,140,161,2,96,134,195
data 132,196,108,48,3,133,147,169,0,133,144,165,186,208,3,76
data 19,247,201,3,240,249,144,123,164,183,208,3,76,16,247,166
data 185,32,175,245,169,96,133,185,32,213,243,165,186,32,9,237
data 165,185,32,199,237,32,19,238,133,174,165,144,74,74,176,80
data 32,19,238,133,175,138,208,8,165,195,133,174,165,196,133,175
data 32,210,245,169,253,37,144,133,144,32,225,255,208,3,76,51
data 246,32,19,238,170,165,144,74,74,176,232,138,164,147,240,12
data 160,0,209,174,240,8,169,16,32,28,254,44,145,174,230,174
data 208,2,230,175,36,144,80,203,32,239,237,32,66,246,144,121
data 76,4,247,74,176,3,76,19,247,32,208,247,176,3,76,19
data 247,32,23,248,176,104,32,175,245,165,183,240,9,32,234,247
data 144,11,240,90,176,218,32,44,247,240,83,176,211,165,144,41
data 16,56,208,74,224,1,240,17,224,3,208,221,160,1,177,178
data 133,195,200,177,178,133,196,176,4,165,185,208,239,160,3,177
data 178,160,1,241,178,170,160,4,177,178,160,2,241,178,168,24
data 138,101,195,133,174,152,101,196,133,175,165,195,133,193,165,196
data 133,194,32,210,245,32,74,248,36,24,166,174,164,175,96,165
data 157,16,30,160,12,32,47,241,165,183,240,21,160,23,32,47
data 241,164,183,240,12,160,0,177,187,32,210,255,200,196,183,208
data 246,96,160,73,165,147,240,2,160,89,76,43,241,134,174,132
data 175,170,181,0,133,193,181,1,133,194,108,50,3,165,186,208
data 3,76,19,247,201,3,240,249,144,95,169,97,133,185,164,183
data 208,3,76,16,247,32,213,243,32,143,246,165,186,32,12,237
data 165,185,32,185,237,160,0,32,142,251,165,172,32,221,237,165
data 173,32,221,237,32,209,252,176,22,177,172,32,221,237,32,225
data 255,208,7,32,66,246,169,0,56,96,32,219,252,208,229,32
data 254,237,36,185,48,17,165,186,32,12,237,165,185,41,239,9
data 224,32,185,237,32,254,237,24,96,74,176,3,76,19,247,32
data 208,247,144,141,32,56,248,176,37,32,143,246,162,3,165,185
data 41,1,208,2,162,1,138,32,106,247,176,18,32,103,248,176
data 13,165,185,41,2,240,6,169,5,32,106,247,36,24,96,165
data 157,16,251,160,81,32,47,241,76,193,245,162,0,230,162,208
data 6,230,161,208,2,230,160,56,165,162,233,1,165,161,233,26
data 165,160,233,79,144,6,134,160,134,161,134,162,173,1,220,205
data 1,220,208,248,170,48,19,162,189,142,0,220,174,1,220,236
data 1,220,208,248,141,0,220,232,208,2,133,145,96,120,165,162
data 166,161,164,160,120,133,162,134,161,132,160,88,96,165,145,201
data 127,208,7,8,32,204,255,133,198,40,96,169,1,44,169,2
data 44,169,3,44,169,4,44,169,5,44,169,6,44,169,7,44
data 169,8,44,169,9,72,32,204,255,160,0,36,157,80,10,32
data 47,241,104,72,9,48,32,210,255,104,56,96,165,147,72,32
data 65,248,104,133,147,176,50,160,0,177,178,201,5,240,42,201
data 1,240,8,201,3,240,4,201,4,208,225,170,36,157,16,23
data 160,99,32,47,241,160,5,177,178,32,210,255,200,192,21,208
data 246,165,161,32,224,228,234,24,136,96,133,158,32,208,247,144
data 94,165,194,72,165,193,72,165,175,72,165,174,72,160,191,169
data 32,145,178,136,208,251,165,158,145,178,200,165,193,145,178,200
data 165,194,145,178,200,165,174,145,178,200,165,175,145,178,200,132
data 159,160,0,132,158,164,158,196,183,240,12,177,187,164,159,145
data 178,230,158,230,159,208,238,32,215,247,169,105,133,171,32,107
data 248,168,104,133,174,104,133,175,104,133,193,104,133,194,152,96
data 166,178,164,179,192,2,96,32,208,247,138,133,193,24,105,192
data 133,174,152,133,194,105,0,133,175,96,32,44,247,176,29,160
data 5,132,159,160,0,132,158,196,183,240,16,177,187,164,159,209
data 178,208,231,230,158,230,159,164,158,208,236,24,96,32,208,247
data 230,166,164,166,192,192,96,32,46,248,240,26,160,27,32,47
data 241,32,208,248,32,46,248,208,248,160,106,76,47,241,169,16
data 36,1,208,2,36,1,24,96,32,46,248,240,249,160,46,208
data 221,169,0,133,144,133,147,32,215,247,32,23,248,176,31,120
data 169,0,133,170,133,180,133,176,133,158,133,159,133,156,169,144
data 162,14,208,17,32,215,247,169,20,133,171,32,56,248,176,108
data 120,169,130,162,8,160,127,140,13,220,141,13,220,173,14,220
data 9,25,141,15,220,41,145,141,162,2,32,164,240,173,17,208
data 41,239,141,17,208,173,20,3,141,159,2,173,21,3,141,160
data 2,32,189,252,169,2,133,190,32,151,251,165,1,41,31,133
data 1,133,192,162,255,160,255,136,208,253,202,208,248,88,173,160
data 2,205,21,3,24,240,21,32,208,248,32,188,246,76,190,248
data 32,225,255,24,208,11,32,147,252,56,104,104,169,0,141,160
data 2,96,134,177,165,176,10,10,24,101,176,24,101,177,133,177
data 169,0,36,176,48,1,42,6,177,42,6,177,42,170,173,6
data 220,201,22,144,249,101,177,141,4,220,138,109,7,220,141,5
data 220,173,162,2,141,14,220,141,164,2,173,13,220,41,16,240
data 9,169,249,72,169,42,72,76,67,255,88,96,174,7,220,160
data 255,152,237,6,220,236,7,220,208,242,134,177,170,140,6,220
data 140,7,220,169,25,141,15,220,173,13,220,141,163,2,152,229
data 177,134,177,74,102,177,74,102,177,165,176,24,105,60,197,177
data 176,74,166,156,240,3,76,96,250,166,163,48,27,162,0,105
data 48,101,176,197,177,176,28,232,105,38,101,176,197,177,176,23
data 105,44,101,176,197,177,144,3,76,16,250,165,180,240,29,133
data 168,208,25,230,169,176,2,198,169,56,233,19,229,177,101,146
data 133,146,165,164,73,1,133,164,240,43,134,215,165,180,240,34
data 173,163,2,41,1,208,5,173,164,2,208,22,169,0,133,164
data 141,164,2,165,163,16,48,48,191,162,166,32,226,248,165,155
data 208,185,76,188,254,165,146,240,7,48,3,198,176,44,230,176
data 169,0,133,146,228,215,208,15,138,208,160,165,169,48,189,201
data 16,144,185,133,150,176,181,138,69,155,133,155,165,180,240,210
data 198,163,48,197,70,215,102,191,162,218,32,226,248,76,188,254
data 165,150,240,4,165,180,240,7,165,163,48,3,76,151,249,70
data 177,169,147,56,229,177,101,176,10,170,32,226,248,230,156,165
data 180,208,17,165,150,240,38,133,168,169,0,133,150,169,129,141
data 13,220,133,180,165,150,133,181,240,9,169,0,133,180,169,1
data 141,13,220,165,191,133,189,165,168,5,169,133,182,76,188,254
data 32,151,251,133,156,162,218,32,226,248,165,190,240,2,133,167
data 169,15,36,170,16,23,165,181,208,12,166,190,202,208,11,169
data 8,32,28,254,208,4,169,0,133,170,76,188,254,112,49,208
data 24,165,181,208,245,165,182,208,241,165,167,74,165,189,48,3
data 144,24,24,176,21,41,15,133,170,198,170,208,221,169,64,133
data 170,32,142,251,169,0,133,171,240,208,169,128,133,170,208,202
data 165,181,240,10,169,4,32,28,254,169,0,76,74,251,32,209
data 252,144,3,76,72,251,166,167,202,240,45,165,147,240,12,160
data 0,165,189,209,172,240,4,169,1,133,182,165,182,240,75,162
data 61,228,158,144,62,166,158,165,173,157,1,1,165,172,157,0
data 1,232,232,134,158,76,58,251,166,159,228,158,240,53,165,172
data 221,0,1,208,46,165,173,221,1,1,208,39,230,159,230,159
data 165,147,240,11,165,189,160,0,209,172,240,23,200,132,182,165
data 182,240,7,169,16,32,28,254,208,9,165,147,208,5,168,165
data 189,145,172,32,219,252,208,67,169,128,133,170,120,162,1,142
data 13,220,174,13,220,166,190,202,48,2,134,190,198,167,240,8
data 165,158,208,39,133,190,240,35,32,147,252,32,142,251,160,0
data 132,171,177,172,69,171,133,171,32,219,252,32,209,252,144,242
data 165,171,69,189,240,5,169,32,32,28,254,76,188,254,165,194
data 133,173,165,193,133,172,96,169,8,133,163,169,0,133,164,133
data 168,133,155,133,169,96,165,189,74,169,96,144,2,169,176,162
data 0,141,6,220,142,7,220,173,13,220,169,25,141,15,220,165
data 1,73,8,133,1,41,8,96,56,102,182,48,60,165,168,208
data 18,169,16,162,1,32,177,251,208,47,230,168,165,182,16,41
data 76,87,252,165,169,208,9,32,173,251,208,29,230,169,208,25
data 32,166,251,208,20,165,164,73,1,133,164,240,15,165,189,73
data 1,133,189,41,1,69,155,133,155,76,188,254,70,189,198,163
data 165,163,240,58,16,243,32,151,251,88,165,165,240,18,162,0
data 134,215,198,165,166,190,224,2,208,2,9,128,133,189,208,217
data 32,209,252,144,10,208,145,230,173,165,215,133,189,176,202,160
data 0,177,172,133,189,69,215,133,215,32,219,252,208,187,165,155
data 73,1,133,189,76,188,254,198,190,208,3,32,202,252,169,80
data 133,167,162,8,120,32,189,252,208,234,169,120,32,175,251,208
data 227,198,167,208,223,32,151,251,198,171,16,216,162,10,32,189
data 252,88,230,171,165,190,240,48,32,142,251,162,9,134,165,134
data 182,208,131,8,120,173,17,208,9,16,141,17,208,32,202,252
data 169,127,141,13,220,32,221,253,173,160,2,240,9,141,21,3
data 173,159,2,141,20,3,40,96,32,147,252,240,151,189,147,253
data 141,20,3,189,148,253,141,21,3,96,165,1,9,32,133,1
data 96,56,165,172,229,174,165,173,229,175,96,230,172,208,2,230
data 173,96,162,255,120,154,216,32,2,253,208,3,108,0,128,142
data 22,208,32,163,253,32,80,253,32,21,253,32,91,255,88,108
data 0,160,162,5,189,15,253,221,3,128,208,3,202,208,245,96
data 195,194,205,56,48,162,48,160,253,24,134,195,132,196,160,31
data 185,20,3,176,2,177,195,145,195,153,20,3,136,16,241,96
data 49,234,102,254,71,254,74,243,145,242,14,242,80,242,51,243
data 87,241,202,241,237,246,62,241,47,243,102,254,165,244,237,245
data 169,0,168,153,2,0,153,0,2,153,0,3,200,208,244,162
data 60,160,3,134,178,132,179,168,169,3,133,194,230,194,177,193
data 170,169,85,145,193,209,193,208,15,42,145,193,209,193,208,8
data 138,145,193,200,208,232,240,228,152,170,164,194,24,32,45,254
data 169,8,141,130,2,169,4,141,136,2,96,106,252,205,251,49
data 234,44,249,169,127,141,13,220,141,13,221,141,0,220,169,8
data 141,14,220,141,14,221,141,15,220,141,15,221,162,0,142,3
data 220,142,3,221,142,24,212,202,142,2,220,169,7,141,0,221
data 169,63,141,2,221,169,231,133,1,169,47,133,0,173,166,2
data 240,10,169,37,141,4,220,169,64,76,243,253,169,149,141,4
data 220,169,66,141,5,220,76,110,255,133,183,134,187,132,188,96
data 133,184,134,186,132,185,96,165,186,201,2,208,13,173,151,2
data 72,169,0,141,151,2,104,96,133,157,165,144,5,144,133,144
data 96,141,133,2,96,144,6,174,131,2,172,132,2,142,131,2
data 140,132,2,96,144,6,174,129,2,172,130,2,142,129,2,140
data 130,2,96,120,108,24,3,72,138,72,152,72,169,127,141,13
data 221,172,13,221,48,28,32,2,253,208,3,108,2,128,32,188
data 246,32,225,255,208,12,32,21,253,32,163,253,32,24,229,108
data 2,160,152,45,161,2,170,41,1,240,40,173,0,221,41,251
data 5,181,141,0,221,173,161,2,141,13,221,138,41,18,240,13
data 41,2,240,6,32,214,254,76,157,254,32,7,255,32,187,238
data 76,182,254,138,41,2,240,6,32,214,254,76,182,254,138,41
data 16,240,3,32,7,255,173,161,2,141,13,221,104,168,104,170
data 104,64,193,39,62,26,197,17,116,14,237,12,69,6,240,2
data 70,1,184,0,113,0,173,1,221,41,1,133,167,173,6,221
data 233,28,109,153,2,141,6,221,173,7,221,109,154,2,141,7
data 221,169,17,141,15,221,173,161,2,141,13,221,169,255,141,6
data 221,141,7,221,76,89,239,173,149,2,141,6,221,173,150,2
data 141,7,221,169,17,141,15,221,169,18,77,161,2,141,161,2
data 169,255,141,6,221,141,7,221,174,152,2,134,168,96,170,173
data 150,2,42,168,138,105,200,141,153,2,152,105,0,141,154,2
data 96,234,234,8,104,41,239,72,72,138,72,152,72,186,189,4
data 1,41,16,240,3,108,22,3,108,20,3,32,24,229,173,18
data 208,208,251,173,25,208,41,1,141,166,2,76,221,253,169,129
data 141,13,220,173,14,220,41,128,9,17,141,14,220,76,142,238
data 0,76,91,255,76,163,253,76,80,253,76,21,253,76,26,253
data 76,24,254,76,185,237,76,199,237,76,37,254,76,52,254,76
data 135,234,76,33,254,76,19,238,76,221,237,76,239,237,76,254
data 237, 76, 12,237, 76,  9,237, 76,  7,254, 76,  0,254, 76,249,253
data 108, 26,  3,108, 28,  3,108, 30,  3,108, 32,  3,108, 34,  3,108
data  36,  3,108, 38,  3, 76,158,244, 76,221,245, 76,228,246, 76,221
data 246,108, 40,  3,108, 42,  3,108, 44,  3, 76,155,246, 76,  5,229
data  76, 10,229, 76,  0,229, 82, 82, 66, 89, 67,254,226,252, 72,255

BASIC_ROM:
data 148,227,123,227, 67, 66, 77, 66, 65, 83, 73, 67, 48,168, 65,167
data  29,173,247,168,164,171,190,171,128,176,  5,172,164,169,159,168
data 112,168, 39,169, 28,168,130,168,209,168, 58,169, 46,168, 74,169
data  44,184,103,225, 85,225,100,225,178,179, 35,184,127,170,159,170
data  86,168,155,166, 93,166,133,170, 41,225,189,225,198,225,122,171
data  65,166, 57,188,204,188, 88,188, 16,  3,125,179,158,179,113,191
data 151,224,234,185,237,191,100,226,107,226,180,226, 14,227, 13,184
data 124,183,101,180,173,183,139,183,236,182,  0,183, 44,183, 55,183
data 121,105,184,121, 82,184,123, 42,186,123, 17,187,127,122,191, 80
data 232,175, 70,229,175,125,179,191, 90,211,174,100, 21,176, 69, 78
data 196, 70, 79,210, 78, 69, 88,212, 68, 65, 84,193, 73, 78, 80, 85
data  84,163, 73, 78, 80, 85,212, 68, 73,205, 82, 69, 65,196, 76, 69
data 212, 71, 79, 84,207, 82, 85,206, 73,198, 82, 69, 83, 84, 79, 82
data 197, 71, 79, 83, 85,194, 82, 69, 84, 85, 82,206, 82, 69,205, 83
data  84, 79,208, 79,206, 87, 65, 73,212, 76, 79, 65,196, 83, 65, 86
data 197, 86, 69, 82, 73, 70,217, 68, 69,198, 80, 79, 75,197, 80, 82
data  73, 78, 84,163, 80, 82, 73, 78,212, 67, 79, 78,212, 76, 73, 83
data 212, 67, 76,210, 67, 77,196, 83, 89,211, 79, 80, 69,206, 67, 76
data  79, 83,197, 71, 69,212, 78, 69,215, 84, 65, 66,168, 84,207, 70
data 206, 83, 80, 67,168, 84, 72, 69,206, 78, 79,212, 83, 84, 69,208
data 171,173,170,175,222, 65, 78,196, 79,210,190,189,188, 83, 71,206
data  73, 78,212, 65, 66,211, 85, 83,210, 70, 82,197, 80, 79,211, 83
data  81,210, 82, 78,196, 76, 79,199, 69, 88,208, 67, 79,211, 83, 73
data 206, 84, 65,206, 65, 84,206, 80, 69, 69,203, 76, 69,206, 83, 84
data  82,164,86,65,204,65,83,195,67,72,82,164,76,69,70,84
data 164,82,73,71,72,84,164,77,73,68,164,71,207,0,84,79
data  79,32,77,65,78,89,32,70,73,76,69,211,70,73,76,69
data  32,79,80,69,206,70,73,76,69,32,78,79,84,32,79,80
data  69,206,70,73,76,69,32,78,79,84,32,70,79,85,78,196
data  68,69,86,73,67,69,32,78,79,84,32,80,82,69,83,69
data  78,212,78,79,84,32,73,78,80,85,84,32,70,73,76,197
data  78,79,84,32,79,85,84,80,85,84,32,70,73,76,197,77
data  73,83,83,73,78,71,32,70,73,76,69,32,78,65,77,197
data  73,76,76,69,71,65,76,32,68,69,86,73,67,69,32,78
data  85,77,66,69,210,78,69,88,84,32,87,73,84,72,79,85
data  84,32,70,79,210,83,89,78,84,65,216,82,69,84,85,82
data  78,32,87,73,84,72,79,85,84,32,71,79,83,85,194,79
data  85,84,32,79,70,32,68,65,84,193,73,76,76,69,71,65
data  76,32,81,85,65,78,84,73,84,217,79,86,69,82,70,76
data  79,215,79,85,84,32,79,70,32,77,69,77,79,82,217,85
data  78,68,69,70,39,68,32,83,84,65,84,69,77,69,78,212
data  66,65,68,32,83,85,66,83,67,82,73,80,212,82,69,68
data  73,77,39,68,32,65,82,82,65,217,68,73,86,73,83,73
data  79,78,32,66,89,32,90,69,82,207,73,76,76,69,71,65
data  76,32,68,73,82,69,67,212,84,89,80,69,32,77,73,83
data  77,65,84,67,200,83,84,82,73,78,71,32,84,79,79,32
data  76,79,78,199,70,73,76,69,32,68,65,84,193,70,79,82
data  77,85,76,65,32,84,79,79,32,67,79,77,80,76,69,216
data  67,65,78,39,84,32,67,79,78,84,73,78,85,197,85,78
data  68,69,70,39,68,32,70,85,78,67,84,73,79,206,86,69
data  82,73,70,217,76,79,65,196,158,161,172,161,181,161,194,161
data 208,161,226,161,240,161,255,161,16,162,37,162,53,162,59,162
data 79,162,90,162,106,162,114,162,127,162,144,162,157,162,170,162
data 186,162,200,162,213,162,228,162,237,162,0,163,14,163,30,163
data 36,163,131,163,13,79,75,13,0,32,32,69,82,82,79,82
data 0,32,73,78,32,0,13,10,82,69,65,68,89,46,13,10
data 0,13,10,66,82,69,65,75,0,160,186,232,232,232,232,189
data 1,1,201,129,208,33,165,74,208,10,189,2,1,133,73,189
data 3,1,133,74,221,3,1,208,7,165,73,221,2,1,240,7
data 138,24,105,18,170,208,216,96,32,8,164,133,49,132,50,56
data 165,90,229,95,133,34,168,165,91,229,96,170,232,152,240,35
data 165,90,56,229,34,133,90,176,3,198,91,56,165,88,229,34
data 133,88,176,8,198,89,144,4,177,90,145,88,136,208,249,177
data 90,145,88,198,91,198,89,202,208,242,96,10,105,62,176,53
data 133,34,186,228,34,144,46,96,196,52,144,40,208,4,197,51
data 144,34,72,162,9,152,72,181,87,202,16,250,32,38,181,162
data 247,104,149,97,232,48,250,104,168,104,196,52,144,6,208,5
data 197,51,176,1,96,162,16,108,0,3,138,10,170,189,38,163
data 133,34,189,39,163,133,35,32,204,255,169,0,133,19,32,215
data 170,32,69,171,160,0,177,34,72,41,127,32,71,171,200,104
data 16,244,32,122,166,169,105,160,163,32,30,171,164,58,200,240
data 3,32,194,189,169,118,160,163,32,30,171,169,128,32,144,255
data 108,2,3,32,96,165,134,122,132,123,32,115,0,170,240,240
data 162,255,134,58,144,6,32,121,165,76,225,167,32,107,169,32
data 121,165,132,11,32,19,166,144,68,160,1,177,95,133,35,165
data 45,133,34,165,96,133,37,165,95,136,241,95,24,101,45,133
data 45,133,36,165,46,105,255,133,46,229,96,170,56,165,95,229
data 45,168,176,3,232,198,37,24,101,34,144,3,198,35,24,177
data 34,145,36,200,208,249,230,35,230,37,202,208,242,32,89,166
data 32,51,165,173,0,2,240,136,24,165,45,133,90,101,11,133
data 88,164,46,132,91,144,1,200,132,89,32,184,163,165,20,164
data 21,141,254,1,140,255,1,165,49,164,50,133,45,132,46,164
data 11,136,185,252,1,145,95,136,16,248,32,89,166,32,51,165
data 76,128,164,165,43,164,44,133,34,132,35,24,160,1,177,34
data 240,29,160,4,200,177,34,208,251,200,152,101,34,170,160,0
data 145,34,165,35,105,0,200,145,34,134,34,133,35,144,221,96
data 162,0,32,18,225,201,13,240,13,157,0,2,232,224,89,144
data 241,162,23,76,55,164,76,202,170,108,4,3,166,122,160,4
data 132,15,189,0,2,16,7,201,255,240,62,232,208,244,201,32
data 240,55,133,8,201,34,240,86,36,15,112,45,201,63,208,4
data 169,153,208,37,201,48,144,4,201,60,144,29,132,113,160,0
data 132,11,136,134,122,202,200,232,189,0,2,56,249,158,160,240
data 245,201,128,208,48,5,11,164,113,232,200,153,251,1,185,251
data 1,240,54,56,233,58,240,4,201,73,208,2,133,15,56,233
data 85,208,159,133,8,189,0,2,240,223,197,8,240,219,200,153
data 251,1,232,208,240,166,122,230,11,200,185,157,160,16,250,185
data 158,160,208,180,189,0,2,16,190,153,253,1,198,123,169,255
data 133,122,96,165,43,166,44,160,1,133,95,134,96,177,95,240
data 31,200,200,165,21,209,95,144,24,240,3,136,208,9,165,20
data 136,209,95,144,12,240,10,136,177,95,170,136,177,95,176,215
data 24,96,208,253,169,0,168,145,43,200,145,43,165,43,24,105
data 2,133,45,165,44,105,0,133,46,32,142,166,169,0,208,45
data 32,231,255,165,55,164,56,133,51,132,52,165,45,164,46,133
data 47,132,48,133,49,132,50,32,29,168,162,25,134,22,104,168
data 104,162,250,154,72,152,72,169,0,133,62,133,16,96,24,165
data 43,105,255,133,122,165,44,105,255,133,123,96,144,6,240,4
data 201,171,208,233,32,107,169,32,19,166,32,121,0,240,12,201
data 171,208,142,32,115,0,32,107,169,208,134,104,104,165,20,5
data 21,208,6,169,255,133,20,133,21,160,1,132,15,177,95,240
data 67,32,44,168,32,215,170,200,177,95,170,200,177,95,197,21
data 208,4,228,20,240,2,176,44,132,73,32,205,189,169,32,164
data 73,41,127,32,71,171,201,34,208,6,165,15,73,255,133,15
data 200,240,17,177,95,208,16,168,177,95,170,200,177,95,134,95
data 133,96,208,181,76,134,227,108,6,3,16,215,201,255,240,211
data 36,15,48,207,56,233,127,170,132,73,160,255,202,240,8,200
data 185,158,160,16,250,48,245,200,185,158,160,48,178,32,71,171
data 208,245,169,128,133,16,32,165,169,32,138,163,208,5,138,105
data 15,170,154,104,104,169,9,32,251,163,32,6,169,24,152,101
data 122,72,165,123,105,0,72,165,58,72,165,57,72,169,164,32
data 255,174,32,141,173,32,138,173,165,102,9,127,37,98,133,98
data 169,139,160,167,133,34,132,35,76,67,174,169,188,160,185,32
data 162,187,32,121,0,201,169,208,6,32,115,0,32,138,173,32
data 43,188,32,56,174,165,74,72,165,73,72,169,129,72,32,44
data 168,165,122,164,123,192,2,234,240,4,133,61,132,62,160,0
data 177,122,208,67,160,2,177,122,24,208,3,76,75,168,200,177
data 122,133,57,200,177,122,133,58,152,101,122,133,122,144,2,230
data 123,108,8,3,32,115,0,32,237,167,76,174,167,240,60,233
data 128,144,17,201,35,176,23,10,168,185,13,160,72,185,12,160
data 72,76,115,0,76,165,169,201,58,240,214,76,8,175,201,75
data 208,249,32,115,0,169,164,32,255,174,76,160,168,56,165,43
data 233,1,164,44,176,1,136,133,65,132,66,96,32,225,255,176
data 1,24,208,60,165,122,164,123,166,58,232,240,12,133,61,132
data 62,165,57,164,58,133,59,132,60,104,104,169,129,160,163,144
data 3,76,105,164,76,134,227,208,23,162,26,164,62,208,3,76
data 55,164,165,61,133,122,132,123,165,59,164,60,133,57,132,58
data 96,8,169,0,32,144,255,40,208,3,76,89,166,32,96,166
data 76,151,168,169,3,32,251,163,165,123,72,165,122,72,165,58
data 72,165,57,72,169,141,72,32,121,0,32,160,168,76,174,167
data 32,107,169,32,9,169,56,165,57,229,20,165,58,229,21,176
data 11,152,56,101,122,166,123,144,7,232,176,4,165,43,166,44
data 32,23,166,144,30,165,95,233,1,133,122,165,96,233,0,133
data 123,96,208,253,169,255,133,74,32,138,163,154,201,141,240,11
data 162,12,44,162,17,76,55,164,76,8,175,104,104,133,57,104
data 133,58,104,133,122,104,133,123,32,6,169,152,24,101,122,133
data 122,144,2,230,123,96,162,58,44,162,0,134,7,160,0,132
data 8,165,8,166,7,133,7,134,8,177,122,240,232,197,8,240
data 228,200,201,34,208,243,240,233,32,158,173,32,121,0,201,137
data 240,5,169,167,32,255,174,165,97,208,5,32,9,169,240,187
data 32,121,0,176,3,76,160,168,76,237,167,32,158,183,72,201
data 141,240,4,201,137,208,145,198,101,208,4,104,76,239,167,32
data 115,0,32,107,169,201,44,240,238,104,96,162,0,134,20,134
data 21,176,247,233,47,133,7,165,21,133,34,201,25,176,212,165
data 20,10,38,34,10,38,34,101,20,133,20,165,34,101,21,133
data 21,6,20,38,21,165,20,101,7,133,20,144,2,230,21,32
data 115,0,76,113,169,32,139,176,133,73,132,74,169,178,32,255
data 174,165,14,72,165,13,72,32,158,173,104,42,32,144,173,208
data 24,104,16,18,32,27,188,32,191,177,160,0,165,100,145,73
data 200,165,101,145,73,96,76,208,187,104,164,74,192,191,208,76
data 32,166,182,201,6,208,61,160,0,132,97,132,102,132,113,32
data 29,170,32,226,186,230,113,164,113,32,29,170,32,12,188,170
data 240,5,232,138,32,237,186,164,113,200,192,6,208,223,32,226
data 186,32,155,188,166,100,164,99,165,101,76,219,255,177,34,32
data 128,0,144,3,76,72,178,233,47,76,126,189,160,2,177,100
data 197,52,144,23,208,7,136,177,100,197,51,144,14,164,101,196
data 46,144,8,208,13,165,100,197,45,176,7,165,100,164,101,76
data 104,170,160,0,177,100,32,117,180,165,80,164,81,133,111,132
data 112,32,122,182,169,97,160,0,133,80,132,81,32,219,182,160
data 0,177,80,145,73,200,177,80,145,73,200,177,80,145,73,96
data 32,134,170,76,181,171,32,158,183,240,5,169,44,32,255,174
data 8,134,19,32,24,225,40,76,160,170,32,33,171,32,121,0
data 240,53,240,67,201,163,240,80,201,166,24,240,75,201,44,240
data 55,201,59,240,94,32,158,173,36,13,48,222,32,221,189,32
data 135,180,32,33,171,32,59,171,208,211,169,0,157,0,2,162
data 255,160,1,165,19,208,16,169,13,32,71,171,36,19,16,5
data 169,10,32,71,171,73,255,96,56,32,240,255,152,56,233,10
data 176,252,73,255,105,1,208,22,8,56,32,240,255,132,9,32
data 155,183,201,41,208,89,40,144,6,138,229,9,144,5,170,232
data 202,208,6,32,115,0,76,162,170,32,59,171,208,242,32,135
data 180,32,166,182,170,160,0,232,202,240,188,177,34,32,71,171
data 200,201,13,208,243,32,229,170,76,40,171,165,19,240,3,169
data 32,44,169,29,44,169,63,32,12,225,41,255,96,165,17,240
data 17,48,4,160,255,208,4,165,63,164,64,133,57,132,58,76
data 8,175,165,19,240,5,162,24,76,55,164,169,12,160,173,32
data 30,171,165,61,164,62,133,122,132,123,96,32,166,179,201,35
data 208,16,32,115,0,32,158,183,169,44,32,255,174,134,19,32
data 30,225,162,1,160,2,169,0,141,1,2,169,64,32,15,172
data 166,19,208,19,96,32,158,183,169,44,32,255,174,134,19,32
data 30,225,32,206,171,165,19,32,204,255,162,0,134,19,96,201
data 34,208,11,32,189,174,169,59,32,255,174,32,33,171,32,166
data 179,169,44,141,255,1,32,249,171,165,19,240,13,32,183,255
data 41,2,240,6,32,181,171,76,248,168,173,0,2,208,30,165
data 19,208,227,32,6,169,76,251,168,165,19,208,6,32,69,171
data 32,59,171,76,96,165,166,65,164,66,169,152,44,169,0,133
data 17,134,67,132,68,32,139,176,133,73,132,74,165,122,164,123
data 133,75,132,76,166,67,164,68,134,122,132,123,32,121,0,208
data 32,36,17,80,12,32,36,225,141,0,2,162,255,160,1,208
data 12,48,117,165,19,208,3,32,69,171,32,249,171,134,122,132
data 123,32,115,0,36,13,16,49,36,17,80,9,232,134,122,169
data 0,133,7,240,12,133,7,201,34,240,7,169,58,133,7,169
data 44,24,133,8,165,122,164,123,105,0,144,1,200,32,141,180
data 32,226,183,32,218,169,76,145,172,32,243,188,165,14,32,194
data 169,32,121,0,240,7,201,44,240,3,76,77,171,165,122,164
data 123,133,67,132,68,165,75,164,76,133,122,132,123,32,121,0
data 240,45,32,253,174,76,21,172,32,6,169,200,170,208,18,162
data 13,200,177,122,240,108,200,177,122,133,63,200,177,122,200,133
data 64,32,251,168,32,121,0,170,224,131,208,220,76,81,172,165
data 67,164,68,166,17,16,3,76,39,168,160,0,177,67,240,11
data 165,19,208,7,169,252,160,172,76,30,171,96,63,69,88,84
data 82,65,32,73,71,78,79,82,69,68,13,0,63,82,69,68
data 79,32,70,82,79,77,32,83,84,65,82,84,13,0,208,4
data 160,0,240,3,32,139,176,133,73,132,74,32,138,163,240,5
data 162,10,76,55,164,154,138,24,105,4,72,105,6,133,36,104
data 160,1,32,162,187,186,189,9,1,133,102,165,73,164,74,32
data 103,184,32,208,187,160,1,32,93,188,186,56,253,9,1,240
data 23,189,15,1,133,57,189,16,1,133,58,189,18,1,133,122
data 189,17,1,133,123,76,174,167,138,105,17,170,154,32,121,0
data 201,44,208,241,32,115,0,32,36,173,32,158,173,24,36,56
data 36,13,48,3,176,3,96,176,253,162,22,76,55,164,166,122
data 208,2,198,123,198,122,162,0,36,72,138,72,169,1,32,251
data 163,32,131,174,169,0,133,77,32,121,0,56,233,177,144,23
data 201,3,176,19,201,1,42,73,1,69,77,197,77,144,97,133
data 77,32,115,0,76,187,173,166,77,208,44,176,123,105,7,144
data 119,101,13,208,3,76,61,182,105,255,133,34,10,101,34,168
data 104,217,128,160,176,103,32,141,173,72,32,32,174,104,164,75
data 16,23,170,240,86,208,95,70,13,138,42,166,122,208,2,198
data 123,198,122,160,27,133,77,208,215,217,128,160,176,72,144,217
data 185,130,160,72,185,129,160,72,32,51,174,165,77,76,169,173
data 76,8,175,165,102,190,128,160,168,104,133,34,230,34,104,133
data 35,152,72,32,27,188,165,101,72,165,100,72,165,99,72,165
data 98,72,165,97,72,108,34,0,160,255,104,240,35,201,100,240
data 3,32,141,173,132,75,104,74,133,18,104,133,105,104,133,106
data 104,133,107,104,133,108,104,133,109,104,133,110,69,102,133,111
data 165,97,96,108,10,3,169,0,133,13,32,115,0,176,3,76
data 243,188,32,19,177,144,3,76,40,175,201,255,208,15,169,168
data 160,174,32,162,187,76,115,0,130,73,15,218,161,201,46,240
data 222,201,171,240,88,201,170,240,209,201,34,208,15,165,122,164
data 123,105,0,144,1,200,32,135,180,76,226,183,201,168,208,19
data 160,24,208,59,32,191,177,165,101,73,255,168,165,100,73,255
data 76,145,179,201,165,208,3,76,244,179,201,180,144,3,76,167
data 175,32,250,174,32,158,173,169,41,44,169,40,44,169,44,160
data 0,209,122,208,3,76,115,0,162,11,76,55,164,160,21,104
data 104,76,250,173,56,165,100,233,0,165,101,233,160,144,8,169
data 162,229,100,169,227,229,101,96,32,139,176,133,100,132,101,166
data 69,164,70,165,13,240,38,169,0,133,112,32,20,175,144,28
data 224,84,208,24,192,201,208,20,32,132,175,132,94,136,132,113
data 160,6,132,93,160,36,32,104,190,76,111,180,96,36,14,16
data 13,160,0,177,100,170,200,177,100,168,138,76,145,179,32,20
data 175,144,45,224,84,208,27,192,73,208,37,32,132,175,152,162
data 160,76,79,188,32,222,255,134,100,132,99,133,101,160,0,132
data 98,96,224,83,208,10,192,84,208,6,32,183,255,76,60,188
data 165,100,164,101,76,162,187,10,72,170,32,115,0,224,143,144
data 32,32,250,174,32,158,173,32,253,174,32,143,173,104,170,165
data 101,72,165,100,72,138,72,32,158,183,104,168,138,72,76,214
data 175,32,241,174,104,168,185,234,159,133,85,185,235,159,133,86
data 32,84,0,76,141,173,160,255,44,160,0,132,11,32,191,177
data 165,100,69,11,133,7,165,101,69,11,133,8,32,252,187,32
data 191,177,165,101,69,11,37,8,69,11,168,165,100,69,11,37
data 7,69,11,76,145,179,32,144,173,176,19,165,110,9,127,37
data 106,133,106,169,105,160,0,32,91,188,170,76,97,176,169,0
data 133,13,198,77,32,166,182,133,97,134,98,132,99,165,108,164
data 109,32,170,182,134,108,132,109,170,56,229,97,240,8,169,1
data 144,4,166,97,169,255,133,102,160,255,232,200,202,208,7,166
data 102,48,15,24,144,12,177,108,209,98,240,239,162,255,176,2
data 162,1,232,138,42,37,18,240,2,169,255,76,60,188,32,253
data 174,170,32,144,176,32,121,0,208,244,96,162,0,32,121,0
data 134,12,133,69,32,121,0,32,19,177,176,3,76,8,175,162
data 0,134,13,134,14,32,115,0,144,5,32,19,177,144,11,170
data 32,115,0,144,251,32,19,177,176,246,201,36,208,6,169,255
data 133,13,208,16,201,37,208,19,165,16,208,208,169,128,133,14
data 5,69,133,69,138,9,128,170,32,115,0,134,70,56,5,16
data 233,40,208,3,76,209,177,160,0,132,16,165,45,166,46,134
data 96,133,95,228,48,208,4,197,47,240,34,165,69,209,95,208
data 8,165,70,200,209,95,240,125,136,24,165,95,105,7,144,225
data 232,208,220,201,65,144,5,233,91,56,233,165,96,104,72,201
data 42,208,5,169,19,160,191,96,165,69,164,70,201,84,208,11
data 192,201,240,239,192,73,208,3,76,8,175,201,83,208,4,192
data 84,240,245,165,47,164,48,133,95,132,96,165,49,164,50,133
data 90,132,91,24,105,7,144,1,200,133,88,132,89,32,184,163
data 165,88,164,89,200,133,47,132,48,160,0,165,69,145,95,200
data 165,70,145,95,169,0,200,145,95,200,145,95,200,145,95,200
data 145,95,200,145,95,165,95,24,105,2,164,96,144,1,200,133
data 71,132,72,96,165,11,10,105,5,101,95,164,96,144,1,200
data 133,88,132,89,96,144,128,0,0,0,32,191,177,165,100,164
data 101,96,32,115,0,32,158,173,32,141,173,165,102,48,13,165
data 97,201,144,144,9,169,165,160,177,32,91,188,208,122,76,155
data 188,165,12,5,14,72,165,13,72,160,0,152,72,165,70,72
data 165,69,72,32,178,177,104,133,69,104,133,70,104,168,186,189
data 2,1,72,189,1,1,72,165,100,157,2,1,165,101,157,1
data 1,200,32,121,0,201,44,240,210,132,11,32,247,174,104,133
data 13,104,133,14,41,127,133,12,166,47,165,48,134,95,133,96
data 197,50,208,4,228,49,240,57,160,0,177,95,200,197,69,208
data 6,165,70,209,95,240,22,200,177,95,24,101,95,170,200,177
data 95,101,96,144,215,162,18,44,162,14,76,55,164,162,19,165
data 12,208,247,32,148,177,165,11,160,4,209,95,208,231,76,234
data 178,32,148,177,32,8,164,160,0,132,114,162,5,165,69,145
data 95,16,1,202,200,165,70,145,95,16,2,202,202,134,113,165
data 11,200,200,200,145,95,162,11,169,0,36,12,80,8,104,24
data 105,1,170,104,105,0,200,145,95,200,138,145,95,32,76,179
data 134,113,133,114,164,34,198,11,208,220,101,89,176,93,133,89
data 168,138,101,88,144,3,200,240,82,32,8,164,133,49,132,50
data 169,0,230,114,164,113,240,5,136,145,88,208,251,198,89,198
data 114,208,245,230,89,56,165,49,229,95,160,2,145,95,165,50
data 200,229,96,145,95,165,12,208,98,200,177,95,133,11,169,0
data 133,113,133,114,200,104,170,133,100,104,133,101,209,95,144,14
data 208,6,200,138,209,95,144,7,76,69,178,76,53,164,200,165
data 114,5,113,24,240,10,32,76,179,138,101,100,170,152,164,34
data 101,101,134,113,198,11,208,202,133,114,162,5,165,69,16,1
data 202,165,70,16,2,202,202,134,40,169,0,32,85,179,138,101
data 88,133,71,152,101,89,133,72,168,165,71,96,132,34,177,95
data 133,40,136,177,95,133,41,169,16,133,93,162,0,160,0,138
data 10,170,152,42,168,176,164,6,113,38,114,144,11,24,138,101
data 40,170,152,101,41,168,176,147,198,93,208,227,96,165,13,240
data 3,32,166,182,32,38,181,56,165,51,229,49,168,165,52,229
data 50,162,0,134,13,133,98,132,99,162,144,76,68,188,56,32
data 240,255,169,0,240,235,166,58,232,208,160,162,21,44,162,27
data 76,55,164,32,225,179,32,166,179,32,250,174,169,128,133,16
data 32,139,176,32,141,173,32,247,174,169,178,32,255,174,72,165
data 72,72,165,71,72,165,123,72,165,122,72,32,248,168,76,79
data 180,169,165,32,255,174,9,128,133,16,32,146,176,133,78,132
data 79,76,141,173,32,225,179,165,79,72,165,78,72,32,241,174
data 32,141,173,104,133,78,104,133,79,160,2,177,78,133,71,170
data 200,177,78,240,153,133,72,200,177,71,72,136,16,250,164,72
data 32,212,187,165,123,72,165,122,72,177,78,133,122,200,177,78
data 133,123,165,72,72,165,71,72,32,138,173,104,133,78,104,133
data 79,32,121,0,240,3,76,8,175,104,133,122,104,133,123,160
data 0,104,145,78,104,200,145,78,104,200,145,78,104,200,145,78
data 104,200,145,78,96,32,141,173,160,0,32,223,189,104,104,169
data 255,160,0,240,18,166,100,164,101,134,80,132,81,32,244,180
data 134,98,132,99,133,97,96,162,34,134,7,134,8,133,111,132
data 112,133,98,132,99,160,255,200,177,111,240,12,197,7,240,4
data 197,8,208,243,201,34,240,1,24,132,97,152,101,111,133,113
data 166,112,144,1,232,134,114,165,112,240,4,201,2,208,11,152
data 32,117,180,166,111,164,112,32,136,182,166,22,224,34,208,5
data 162,25,76,55,164,165,97,149,0,165,98,149,1,165,99,149
data 2,160,0,134,100,132,101,132,112,136,132,13,134,23,232,232
data 232,134,22,96,70,15,72,73,255,56,101,51,164,52,176,1
data 136,196,50,144,17,208,4,197,49,144,11,133,51,132,52,133
data 53,132,54,170,104,96,162,16,165,15,48,182,32,38,181,169
data 128,133,15,104,208,208,166,55,165,56,134,51,133,52,160,0
data 132,79,132,78,165,49,166,50,133,95,134,96,169,25,162,0
data 133,34,134,35,197,22,240,5,32,199,181,240,247,169,7,133
data 83,165,45,166,46,133,34,134,35,228,48,208,4,197,47,240
data 5,32,189,181,240,243,133,88,134,89,169,3,133,83,165,88
data 166,89,228,50,208,7,197,49,208,3,76,6,182,133,34,134
data 35,160,0,177,34,170,200,177,34,8,200,177,34,101,88,133
data 88,200,177,34,101,89,133,89,40,16,211,138,48,208,200,177
data 34,160,0,10,105,5,101,34,133,34,144,2,230,35,166,35
data 228,89,208,4,197,88,240,186,32,199,181,240,243,177,34,48
data 53,200,177,34,16,48,200,177,34,240,43,200,177,34,170,200
data 177,34,197,52,144,6,208,30,228,51,176,26,197,96,144,22
data 208,4,228,95,144,16,134,95,133,96,165,34,166,35,133,78
data 134,79,165,83,133,85,165,83,24,101,34,133,34,144,2,230
data 35,166,35,160,0,96,165,79,5,78,240,245,165,85,41,4
data 74,168,133,85,177,78,101,95,133,90,165,96,105,0,133,91
data 165,51,166,52,133,88,134,89,32,191,163,164,85,200,165,88
data 145,78,170,230,89,165,89,200,145,78,76,42,181,165,101,72
data 165,100,72,32,131,174,32,143,173,104,133,111,104,133,112,160
data 0,177,111,24,113,100,144,5,162,23,76,55,164,32,117,180
data 32,122,182,165,80,164,81,32,170,182,32,140,182,165,111,164
data 112,32,170,182,32,202,180,76,184,173,160,0,177,111,72,200
data 177,111,170,200,177,111,168,104,134,34,132,35,168,240,10,72
data 136,177,34,145,53,152,208,248,104,24,101,53,133,53,144,2
data 230,54,96,32,143,173,165,100,164,101,133,34,132,35,32,219
data 182,8,160,0,177,34,72,200,177,34,170,200,177,34,168,104
data 40,208,19,196,52,208,15,228,51,208,11,72,24,101,51,133
data 51,144,2,230,52,104,134,34,132,35,96,196,24,208,12,197
data 23,208,8,133,22,233,3,133,23,160,0,96,32,161,183,138
data 72,169,1,32,125,180,104,160,0,145,98,104,104,76,202,180
data 32,97,183,209,80,152,144,4,177,80,170,152,72,138,72,32
data 125,180,165,80,164,81,32,170,182,104,168,104,24,101,34,133
data 34,144,2,230,35,152,32,140,182,76,202,180,32,97,183,24
data 241,80,73,255,76,6,183,169,255,133,101,32,121,0,201,41
data 240,6,32,253,174,32,158,183,32,97,183,240,75,202,138,72
data 24,162,0,241,80,176,182,73,255,197,101,144,177,165,101,176
data 173,32,247,174,104,168,104,133,85,104,104,104,170,104,133,80
data 104,133,81,165,85,72,152,72,160,0,138,96,32,130,183,76
data 162,179,32,163,182,162,0,134,13,168,96,32,130,183,240,8
data 160,0,177,34,168,76,162,179,76,72,178,32,115,0,32,138
data 173,32,184,177,166,100,208,240,166,101,76,121,0,32,130,183
data 208,3,76,247,184,166,122,164,123,134,113,132,114,166,34,134
data 122,24,101,34,133,36,166,35,134,123,144,1,232,134,37,160
data 0,177,36,72,152,145,36,32,121,0,32,243,188,104,160,0
data 145,36,166,113,164,114,134,122,132,123,96,32,138,173,32,247
data 183,32,253,174,76,158,183,165,102,48,157,165,97,201,145,176
data 151,32,155,188,165,100,164,101,132,20,133,21,96,165,21,72
data 165,20,72,32,247,183,160,0,177,20,168,104,133,20,104,133
data 21,76,162,179,32,235,183,138,160,0,145,20,96,32,235,183
data 134,73,162,0,32,121,0,240,3,32,241,183,134,74,160,0
data 177,20,69,74,37,73,240,248,96,169,17,160,191,76,103,184
data 32,140,186,165,102,73,255,133,102,69,110,133,111,165,97,76
data 106,184,32,153,185,144,60,32,140,186,208,3,76,252,187,166
data 112,134,86,162,105,165,105,168,240,206,56,229,97,240,36,144
data 18,132,97,164,110,132,102,73,255,105,0,160,0,132,86,162
data 97,208,4,160,0,132,112,201,249,48,199,168,165,112,86,1
data 32,176,185,36,111,16,87,160,97,224,105,240,2,160,105,56
data 73,255,101,86,133,112,185,4,0,245,4,133,101,185,3,0
data 245,3,133,100,185,2,0,245,2,133,99,185,1,0,245,1
data 133,98,176,3,32,71,185,160,0,152,24,166,98,208,74,166
data 99,134,98,166,100,134,99,166,101,134,100,166,112,134,101,132
data 112,105,8,201,32,208,228,169,0,133,97,133,102,96,101,86
data 133,112,165,101,101,109,133,101,165,100,101,108,133,100,165,99
data 101,107,133,99,165,98,101,106,133,98,76,54,185,105,1,6
data 112,38,101,38,100,38,99,38,98,16,242,56,229,97,176,199
data 73,255,105,1,133,97,144,14,230,97,240,66,102,98,102,99
data 102,100,102,101,102,112,96,165,102,73,255,133,102,165,98,73
data 255,133,98,165,99,73,255,133,99,165,100,73,255,133,100,165
data 101,73,255,133,101,165,112,73,255,133,112,230,112,208,14,230
data 101,208,10,230,100,208,6,230,99,208,2,230,98,96,162,15
data 76,55,164,162,37,180,4,132,112,180,3,148,4,180,2,148
data 3,180,1,148,2,164,104,148,1,105,8,48,232,240,230,233
data 8,168,165,112,176,20,22,1,144,2,246,1,118,1,118,1
data 118,2,118,3,118,4,106,200,208,236,24,96,129,0,0,0
data 0,3,127,94,86,203,121,128,19,155,11,100,128,118,56,147
data 22,130,56,170,59,32,128,53,4,243,52,129,53,4,243,52
data 128,128,0,0,0,128,49,114,23,248,32,43,188,240,2,16
data 3,76,72,178,165,97,233,127,72,169,128,133,97,169,214,160
data 185,32,103,184,169,219,160,185,32,15,187,169,188,160,185,32
data 80,184,169,193,160,185,32,67,224,169,224,160,185,32,103,184
data 104,32,126,189,169,229,160,185,32,140,186,208,3,76,139,186
data 32,183,186,169,0,133,38,133,39,133,40,133,41,165,112,32
data 89,186,165,101,32,89,186,165,100,32,89,186,165,99,32,89
data 186,165,98,32,94,186,76,143,187,208,3,76,131,185,74,9
data 128,168,144,25,24,165,41,101,109,133,41,165,40,101,108,133
data 40,165,39,101,107,133,39,165,38,101,106,133,38,102,38,102
data 39,102,40,102,41,102,112,152,74,208,214,96,133,34,132,35
data 160,4,177,34,133,109,136,177,34,133,108,136,177,34,133,107
data 136,177,34,133,110,69,102,133,111,165,110,9,128,133,106,136
data 177,34,133,105,165,97,96,165,105,240,31,24,101,97,144,4
data 48,29,24,44,16,20,105,128,133,97,208,3,76,251,184,165
data 111,133,102,96,165,102,73,255,48,5,104,104,76,247,184,76
data 126,185,32,12,188,170,240,16,24,105,2,176,242,162,0,134
data 111,32,119,184,230,97,240,231,96,132,32,0,0,0,32,12
data 188,169,249,160,186,162,0,134,111,32,162,187,76,18,187,32
data 140,186,240,118,32,27,188,169,0,56,229,97,133,97,32,183
data 186,230,97,240,186,162,252,169,1,164,106,196,98,208,16,164
data 107,196,99,208,10,164,108,196,100,208,4,164,109,196,101,8
data 42,144,9,232,149,41,240,50,16,52,169,1,40,176,14,6
data 109,38,108,38,107,38,106,176,230,48,206,16,226,168,165,109
data 229,101,133,109,165,108,229,100,133,108,165,107,229,99,133,107
data 165,106,229,98,133,106,152,76,79,187,169,64,208,206,10,10
data 10,10,10,10,133,112,40,76,143,187,162,20,76,55,164,165
data 38,133,98,165,39,133,99,165,40,133,100,165,41,133,101,76
data 215,184,133,34,132,35,160,4,177,34,133,101,136,177,34,133
data 100,136,177,34,133,99,136,177,34,133,102,9,128,133,98,136
data 177,34,133,97,132,112,96,162,92,44,162,87,160,0,240,4
data 166,73,164,74,32,27,188,134,34,132,35,160,4,165,101,145
data 34,136,165,100,145,34,136,165,99,145,34,136,165,102,9,127
data 37,98,145,34,136,165,97,145,34,132,112,96,165,110,133,102
data 162,5,181,104,149,96,202,208,249,134,112,96,32,27,188,162
data 6,181,96,149,104,202,208,249,134,112,96,165,97,240,251,6
data 112,144,247,32,111,185,208,242,76,56,185,165,97,240,9,165
data 102,42,169,255,176,2,169,1,96,32,43,188,133,98,169,0
data 133,99,162,136,165,98,73,255,42,169,0,133,101,133,100,134
data 97,133,112,133,102,76,210,184,70,102,96,133,36,132,37,160
data 0,177,36,200,170,240,196,177,36,69,102,48,194,228,97,208
data 33,177,36,9,128,197,98,208,25,200,177,36,197,99,208,18
data 200,177,36,197,100,208,11,200,169,127,197,112,177,36,229,101
data 240, 40,165,102,144,  2, 73,255, 76, 49,188,165, 97,240, 74, 56
data 233,160, 36,102, 16,  9,170,169,255,133,104, 32, 77,185,138,162
data  97,201,249, 16,  6, 32,153,185,132,104, 96,168,165,102, 41,128
data  70, 98,  5, 98,133, 98, 32,176,185,132,104, 96,165, 97,201,160
data 176, 32, 32,155,188,132,112,165,102,132,102, 73,128, 42,169,160
data 133, 97,165,101,133,  7, 76,210,184,133, 98,133, 99,133,100,133
data 101,168, 96,160,  0,162, 10,148, 93,202, 16,251,144, 15,201, 45
data 208,  4,134,103,240,  4,201, 43,208,  5, 32,115,  0,144, 91,201
data  46,240, 46,201, 69,208, 48, 32,115,  0,144, 23,201,171,240, 14
data 201, 45,240, 10,201,170,240,  8,201, 43,240,  4,208,  7,102, 96
data  32,115,  0,144, 92, 36, 96, 16, 14,169,  0, 56,229, 94, 76, 73
data 189,102, 95, 36, 95, 80,195,165, 94, 56,229, 93,133, 94,240, 18
data  16,  9, 32,254,186,230, 94,208,249,240,  7, 32,226,186,198, 94
data 208,249,165,103, 48,  1, 96, 76,180,191, 72, 36, 95, 16,  2,230
data  93, 32,226,186,104, 56,233, 48, 32,126,189, 76, 10,189, 72, 32
data  12,188,104, 32, 60,188,165,110, 69,102,133,111,166, 97, 76,106
data 184,165, 94,201, 10,144,  9,169,100, 36, 96, 48, 17, 76,126,185
data  10, 10, 24,101, 94, 10, 24,160,  0,113,122, 56,233, 48,133, 94
data  76, 48,189,155, 62,188, 31,253,158,110,107, 39,253,158,110,107
data  40,  0,169,113,160,163, 32,218,189,165, 58,166, 57,133, 98,134
data  99,162,144, 56, 32, 73,188, 32,223,189, 76, 30,171,160,  1,169
data  32, 36,102, 16,  2,169, 45,153,255,  0,133,102,132,113,200,169
data  48,166, 97,208,  3, 76,  4,191,169,  0,224,128,240,  2,176,  9
data 169,189,160,189, 32, 40,186,169,247,133, 93,169,184,160,189, 32
data  91,188,240, 30, 16, 18,169,179,160,189, 32, 91,188,240,  2, 16
data  14, 32,226,186,198, 93,208,238, 32,254,186,230, 93,208,220, 32
data  73,184, 32,155,188,162,  1,165, 93, 24,105, 10, 48,  9,201, 11
data 176,  6,105,255,170,169,  2, 56,233,  2,133, 94,134, 93,138,240
data   2, 16, 19,164,113,169, 46,200,153,255,  0,138,240,  6,169, 48
data 200,153,255,  0,132,113,160,  0,162,128,165,101, 24,121, 25,191
data 133,101,165,100,121, 24,191,133,100,165, 99,121, 23,191,133, 99
data 165, 98,121, 22,191,133, 98,232,176,  4, 16,222, 48,  2, 48,218
data 138,144,  4, 73,255,105, 10,105, 47,200,200,200,200,132, 71,164
data 113,200,170, 41,127,153,255,  0,198, 93,208,  6,169, 46,200,153
data 255,  0,132,113,164, 71,138, 73,255, 41,128,170,192, 36,240,  4
data 192, 60,208,166,164,113,185,255,  0,136,201, 48,240,248,201, 46
data 240,  1,200,169, 43,166, 94,240, 46, 16,  8,169,  0, 56,229, 94
data 170,169, 45,153,  1,  1,169, 69,153,  0,  1,138,162, 47, 56,232
data 233, 10,176,251,105, 58,153,  3,  1,138,153,  2,  1,169,  0,153
data   4,  1,240,  8,153,255,  0,169,  0,153,  0,  1,169,  0,160,  1
data  96,128,  0,  0,  0,  0,250, 10, 31,  0,  0,152,150,128,255,240
data 189,192,  0,  1,134,160,255,255,216,240,  0,  0,  3,232,255,255
data 255,156,  0,  0,  0, 10,255,255,255,255,255,223, 10,128,  0,  3
data  75,192,255,255,115, 96,  0,  0, 14, 16,255,255,253,168,  0,  0
data   0, 60,236,170,170,170,170,170,170,170,170,170,170,170,170,170
data 170,170,170,170,170,170,170,170,170,170,170,170,170,170,170,170
data 170, 32, 12,188,169, 17,160,191, 32,162,187,240,112,165,105,208
data   3, 76,249,184,162, 78,160,  0, 32,212,187,165,110, 16, 15, 32
data 204,188,169, 78,160,  0, 32, 91,188,208,  3,152,164,  7, 32,254
data 187,152, 72, 32,234,185,169, 78,160,  0, 32, 40,186, 32,237,191
data 104, 74,144, 10,165, 97,240,  6,165,102, 73,255,133,102, 96,129
data  56,170, 59, 41,  7,113, 52, 88, 62, 86,116, 22,126,179, 27,119
data  47,238,227,133,122, 29,132, 28, 42,124, 99, 89, 88, 10,126,117
data 253,231,198,128, 49,114, 24, 16,129,  0,  0,  0,  0,169,191,160
data 191, 32, 40,186,165,112,105, 80,144,  3, 32, 35,188, 76,  0,224

CHAR_ROM:
data  60,102,110,110, 96, 98, 60,  0, 24, 60,102,126,102,102,102,  0
data 124,102,102,124,102,102,124,  0, 60,102, 96, 96, 96,102, 60,  0
data 120,108,102,102,102,108,120,  0,126, 96, 96,120, 96, 96,126,  0
data 126, 96, 96,120, 96, 96, 96,  0, 60,102, 96,110,102,102, 60,  0
data 102,102,102,126,102,102,102,  0, 60, 24, 24, 24, 24, 24, 60,  0
data  30, 12, 12, 12, 12,108, 56,  0,102,108,120,112,120,108,102,  0
data  96, 96, 96, 96, 96, 96,126,  0, 99,119,127,107, 99, 99, 99,  0
data 102,118,126,126,110,102,102,  0, 60,102,102,102,102,102, 60,  0
data 124,102,102,124, 96, 96, 96,  0, 60,102,102,102,102, 60, 14,  0
data 124,102,102,124,120,108,102,  0, 60,102, 96, 60,  6,102, 60,  0
data 126, 24, 24, 24, 24, 24, 24,  0,102,102,102,102,102,102, 60,  0
data 102,102,102,102,102, 60, 24,  0, 99, 99, 99,107,127,119, 99,  0
data 102,102, 60, 24, 60,102,102,  0,102,102,102, 60, 24, 24, 24,  0
data 126,  6, 12, 24, 48, 96,126,  0, 60, 48, 48, 48, 48, 48, 60,  0
data  12, 18, 48,124, 48, 98,252,  0, 60, 12, 12, 12, 12, 12, 60,  0
data   0, 24, 60,126, 24, 24, 24, 24,  0, 16, 48,127,127, 48, 16,  0
data   0,  0,  0,  0,  0,  0,  0,  0, 24, 24, 24, 24,  0,  0, 24,  0
data 102,102,102,  0,  0,  0,  0,  0,102,102,255,102,255,102,102,  0
data  24, 62, 96, 60,  6,124, 24,  0, 98,102, 12, 24, 48,102, 70,  0
data  60,102, 60, 56,103,102, 63,  0,  6, 12, 24,  0,  0,  0,  0,  0
data  12, 24, 48, 48, 48, 24, 12,  0, 48, 24, 12, 12, 12, 24, 48,  0
data   0,102, 60,255, 60,102,  0,  0,  0, 24, 24,126, 24, 24,  0,  0
data   0,  0,  0,  0,  0, 24, 24, 48,  0,  0,  0,126,  0,  0,  0,  0
data   0,  0,  0,  0,  0, 24, 24,  0,  0,  3,  6, 12, 24, 48, 96,  0
data  60,102,110,118,102,102, 60,  0, 24, 24, 56, 24, 24, 24,126,  0
data  60,102,  6, 12, 48, 96,126,  0, 60,102,  6, 28,  6,102, 60,  0
data   6, 14, 30,102,127,  6,  6,  0,126, 96,124,  6,  6,102, 60,  0
data  60,102, 96,124,102,102, 60,  0,126,102, 12, 24, 24, 24, 24,  0
data  60,102,102, 60,102,102, 60,  0, 60,102,102, 62,  6,102, 60,  0
data   0,  0, 24,  0,  0, 24,  0,  0,  0,  0, 24,  0,  0, 24, 24, 48
data  14, 24, 48, 96, 48, 24, 14,  0,  0,  0,126,  0,126,  0,  0,  0
data 112, 24, 12,  6, 12, 24,112,  0, 60,102,  6, 12, 24,  0, 24,  0
data   0,  0,  0,255,255,  0,  0,  0,  8, 28, 62,127,127, 28, 62,  0
data  24, 24, 24, 24, 24, 24, 24, 24,  0,  0,  0,255,255,  0,  0,  0
data   0,  0,255,255,  0,  0,  0,  0,  0,255,255,  0,  0,  0,  0,  0
data   0,  0,  0,  0,255,255,  0,  0, 48, 48, 48, 48, 48, 48, 48, 48
data  12, 12, 12, 12, 12, 12, 12, 12,  0,  0,  0,224,240, 56, 24, 24
data  24, 24, 28, 15,  7,  0,  0,  0, 24, 24, 56,240,224,  0,  0,  0
data 192,192,192,192,192,192,255,255,192,224,112, 56, 28, 14,  7,  3
data   3,  7, 14, 28, 56,112,224,192,255,255,192,192,192,192,192,192
data 255,255,  3,  3,  3,  3,  3,  3,  0, 60,126,126,126,126, 60,  0
data   0,  0,  0,  0,  0,255,255,  0, 54,127,127,127, 62, 28,  8,  0
data  96, 96, 96, 96, 96, 96, 96, 96,  0,  0,  0,  7, 15, 28, 24, 24
data 195,231,126, 60, 60,126,231,195,  0, 60,126,102,102,126, 60,  0
data  24, 24,102,102, 24, 24, 60,  0,  6,  6,  6,  6,  6,  6,  6,  6
data   8, 28, 62,127, 62, 28,  8,  0, 24, 24, 24,255,255, 24, 24, 24
data 192,192, 48, 48,192,192, 48, 48, 24, 24, 24, 24, 24, 24, 24, 24
data   0,  0,  3, 62,118, 54, 54,  0,255,127, 63, 31, 15,  7,  3,  1
data   0,  0,  0,  0,  0,  0,  0,  0,240,240,240,240,240,240,240,240
data   0,  0,  0,  0,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0
data   0,  0,  0,  0,  0,  0,  0,255,192,192,192,192,192,192,192,192
data 204,204, 51, 51,204,204, 51, 51,  3,  3,  3,  3,  3,  3,  3,  3
data   0,  0,  0,  0,204,204, 51, 51,255,254,252,248,240,224,192,128
data   3,  3,  3,  3,  3,  3,  3,  3, 24, 24, 24, 31, 31, 24, 24, 24
data   0,  0,  0,  0, 15, 15, 15, 15, 24, 24, 24, 31, 31,  0,  0,  0
data   0,  0,  0,248,248, 24, 24, 24,  0,  0,  0,  0,  0,  0,255,255
data   0,  0,  0, 31, 31, 24, 24, 24, 24, 24, 24,255,255,  0,  0,  0
data   0,  0,  0,255,255, 24, 24, 24, 24, 24, 24,248,248, 24, 24, 24
data 192,192,192,192,192,192,192,192,224,224,224,224,224,224,224,224
data   7,  7,  7,  7,  7,  7,  7,  7,255,255,  0,  0,  0,  0,  0,  0
data 255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255
data   3,  3,  3,  3,  3,  3,255,255,  0,  0,  0,  0,240,240,240,240
data  15, 15, 15, 15,  0,  0,  0,  0, 24, 24, 24,248,248,  0,  0,  0
data 240,240,240,240,  0,  0,  0,  0,240,240,240,240, 15, 15, 15, 15
data 195,153,145,145,159,153,195,255,231,195,153,129,153,153,153,255
data 131,153,153,131,153,153,131,255,195,153,159,159,159,153,195,255
data 135,147,153,153,153,147,135,255,129,159,159,135,159,159,129,255
data 129,159,159,135,159,159,159,255,195,153,159,145,153,153,195,255
data 153,153,153,129,153,153,153,255,195,231,231,231,231,231,195,255
data 225,243,243,243,243,147,199,255,153,147,135,143,135,147,153,255
data 159,159,159,159,159,159,129,255,156,136,128,148,156,156,156,255
data 153,137,129,129,145,153,153,255,195,153,153,153,153,153,195,255
data 131,153,153,131,159,159,159,255,195,153,153,153,153,195,241,255
data 131,153,153,131,135,147,153,255,195,153,159,195,249,153,195,255
data 129,231,231,231,231,231,231,255,153,153,153,153,153,153,195,255
data 153,153,153,153,153,195,231,255,156,156,156,148,128,136,156,255
data 153,153,195,231,195,153,153,255,153,153,153,195,231,231,231,255
data 129,249,243,231,207,159,129,255,195,207,207,207,207,207,195,255
data 243,237,207,131,207,157,  3,255,195,243,243,243,243,243,195,255
data 255,231,195,129,231,231,231,231,255,239,207,128,128,207,239,255
data 255,255,255,255,255,255,255,255,231,231,231,231,255,255,231,255
data 153,153,153,255,255,255,255,255,153,153,  0,153,  0,153,153,255
data 231,193,159,195,249,131,231,255,157,153,243,231,207,153,185,255
data 195,153,195,199,152,153,192,255,249,243,231,255,255,255,255,255
data 243,231,207,207,207,231,243,255,207,231,243,243,243,231,207,255
data 255,153,195,  0,195,153,255,255,255,231,231,129,231,231,255,255
data 255,255,255,255,255,231,231,207,255,255,255,129,255,255,255,255
data 255,255,255,255,255,231,231,255,255,252,249,243,231,207,159,255
data 195,153,145,137,153,153,195,255,231,231,199,231,231,231,129,255
data 195,153,249,243,207,159,129,255,195,153,249,227,249,153,195,255
data 249,241,225,153,128,249,249,255,129,159,131,249,249,153,195,255
data 195,153,159,131,153,153,195,255,129,153,243,231,231,231,231,255
data 195,153,153,195,153,153,195,255,195,153,153,193,249,153,195,255
data 255,255,231,255,255,231,255,255,255,255,231,255,255,231,231,207
data 241,231,207,159,207,231,241,255,255,255,129,255,129,255,255,255
data 143,231,243,249,243,231,143,255,195,153,249,243,231,255,231,255
data 255,255,255,  0,  0,255,255,255,247,227,193,128,128,227,193,255
data 231,231,231,231,231,231,231,231,255,255,255,  0,  0,255,255,255
data 255,255,  0,  0,255,255,255,255,255,  0,  0,255,255,255,255,255
data 255,255,255,255,  0,  0,255,255,207,207,207,207,207,207,207,207
data 243,243,243,243,243,243,243,243,255,255,255, 31, 15,199,231,231
data 231,231,227,240,248,255,255,255,231,231,199, 15, 31,255,255,255
data  63, 63, 63, 63, 63, 63,  0,  0, 63, 31,143,199,227,241,248,252
data 252,248,241,227,199,143, 31, 63,  0,  0, 63, 63, 63, 63, 63, 63
data   0,  0,252,252,252,252,252,252,255,195,129,129,129,129,195,255
data 255,255,255,255,255,  0,  0,255,201,128,128,128,193,227,247,255
data 159,159,159,159,159,159,159,159,255,255,255,248,240,227,231,231
data  60, 24,129,195,195,129, 24, 60,255,195,129,153,153,129,195,255
data 231,231,153,153,231,231,195,255,249,249,249,249,249,249,249,249
data 247,227,193,128,193,227,247,255,231,231,231,  0,  0,231,231,231
data  63, 63,207,207, 63, 63,207,207,231,231,231,231,231,231,231,231
data 255,255,252,193,137,201,201,255,  0,128,192,224,240,248,252,254
data 255,255,255,255,255,255,255,255, 15, 15, 15, 15, 15, 15, 15, 15
data 255,255,255,255,  0,  0,  0,  0,  0,255,255,255,255,255,255,255
data 255,255,255,255,255,255,255,  0, 63, 63, 63, 63, 63, 63, 63, 63
data  51, 51,204,204, 51, 51,204,204,252,252,252,252,252,252,252,252
data 255,255,255,255, 51, 51,204,204,  0,  1,  3,  7, 15, 31, 63,127
data 252,252,252,252,252,252,252,252,231,231,231,224,224,231,231,231
data 255,255,255,255,240,240,240,240,231,231,231,224,224,255,255,255
data 255,255,255,  7,  7,231,231,231,255,255,255,255,255,255,  0,  0
data 255,255,255,224,224,231,231,231,231,231,231,  0,  0,255,255,255
data 255,255,255,  0,  0,231,231,231,231,231,231,  7,  7,231,231,231
data  63, 63, 63, 63, 63, 63, 63, 63, 31, 31, 31, 31, 31, 31, 31, 31
data 248,248,248,248,248,248,248,248,  0,  0,255,255,255,255,255,255
data   0,  0,  0,255,255,255,255,255,255,255,255,255,255,  0,  0,  0
data 252,252,252,252,252,252,  0,  0,255,255,255,255, 15, 15, 15, 15
data 240,240,240,240,255,255,255,255,231,231,231,  7,  7,255,255,255
data  15, 15, 15, 15,255,255,255,255, 15, 15, 15, 15,240,240,240,240
data  60,102,110,110, 96, 98, 60,  0,  0,  0, 60,  6, 62,102, 62,  0
data   0, 96, 96,124,102,102,124,  0,  0,  0, 60, 96, 96, 96, 60,  0
data   0,  6,  6, 62,102,102, 62,  0,  0,  0, 60,102,126, 96, 60,  0
data   0, 14, 24, 62, 24, 24, 24,  0,  0,  0, 62,102,102, 62,  6,124
data   0, 96, 96,124,102,102,102,  0,  0, 24,  0, 56, 24, 24, 60,  0
data   0,  6,  0,  6,  6,  6,  6, 60,  0, 96, 96,108,120,108,102,  0
data   0, 56, 24, 24, 24, 24, 60,  0,  0,  0,102,127,127,107, 99,  0
data   0,  0,124,102,102,102,102,  0,  0,  0, 60,102,102,102, 60,  0
data   0,  0,124,102,102,124, 96, 96,  0,  0, 62,102,102, 62,  6,  6
data   0,  0,124,102, 96, 96, 96,  0,  0,  0, 62, 96, 60,  6,124,  0
data   0, 24,126, 24, 24, 24, 14,  0,  0,  0,102,102,102,102, 62,  0
data   0,  0,102,102,102, 60, 24,  0,  0,  0, 99,107,127, 62, 54,  0
data   0,  0,102, 60, 24, 60,102,  0,  0,  0,102,102,102, 62, 12,120
data   0,  0,126, 12, 24, 48,126,  0, 60, 48, 48, 48, 48, 48, 60,  0
data  12, 18, 48,124, 48, 98,252,  0, 60, 12, 12, 12, 12, 12, 60,  0
data   0, 24, 60,126, 24, 24, 24, 24,  0, 16, 48,127,127, 48, 16,  0
data   0,  0,  0,  0,  0,  0,  0,  0, 24, 24, 24, 24,  0,  0, 24,  0
data 102,102,102,  0,  0,  0,  0,  0,102,102,255,102,255,102,102,  0
data  24, 62, 96, 60,  6,124, 24,  0, 98,102, 12, 24, 48,102, 70,  0
data  60,102, 60, 56,103,102, 63,  0,  6, 12, 24,  0,  0,  0,  0,  0
data  12, 24, 48, 48, 48, 24, 12,  0, 48, 24, 12, 12, 12, 24, 48,  0
data   0,102, 60,255, 60,102,  0,  0,  0, 24, 24,126, 24, 24,  0,  0
data   0,  0,  0,  0,  0, 24, 24, 48,  0,  0,  0,126,  0,  0,  0,  0
data   0,  0,  0,  0,  0, 24, 24,  0,  0,  3,  6, 12, 24, 48, 96,  0
data  60,102,110,118,102,102, 60,  0, 24, 24, 56, 24, 24, 24,126,  0
data  60,102,  6, 12, 48, 96,126,  0, 60,102,  6, 28,  6,102, 60,  0
data   6, 14, 30,102,127,  6,  6,  0,126, 96,124,  6,  6,102, 60,  0
data  60,102, 96,124,102,102, 60,  0,126,102, 12, 24, 24, 24, 24,  0
data  60,102,102, 60,102,102, 60,  0, 60,102,102, 62,  6,102, 60,  0
data   0,  0, 24,  0,  0, 24,  0,  0,  0,  0, 24,  0,  0, 24, 24, 48
data  14, 24, 48, 96, 48, 24, 14,  0,  0,  0,126,  0,126,  0,  0,  0
data 112, 24, 12,  6, 12, 24,112,  0, 60,102,  6, 12, 24,  0, 24,  0
data   0,  0,  0,255,255,  0,  0,  0, 24, 60,102,126,102,102,102,  0
data 124,102,102,124,102,102,124,  0, 60,102, 96, 96, 96,102, 60,  0
data 120,108,102,102,102,108,120,  0,126, 96, 96,120, 96, 96,126,  0
data 126, 96, 96,120, 96, 96, 96,  0, 60,102, 96,110,102,102, 60,  0
data 102,102,102,126,102,102,102,  0, 60, 24, 24, 24, 24, 24, 60,  0
data  30, 12, 12, 12, 12,108, 56,  0,102,108,120,112,120,108,102,  0
data  96, 96, 96, 96, 96, 96,126,  0, 99,119,127,107, 99, 99, 99,  0
data 102,118,126,126,110,102,102,  0, 60,102,102,102,102,102, 60,  0
data 124,102,102,124, 96, 96, 96,  0, 60,102,102,102,102, 60, 14,  0
data 124,102,102,124,120,108,102,  0, 60,102, 96, 60,  6,102, 60,  0
data 126, 24, 24, 24, 24, 24, 24,  0,102,102,102,102,102,102, 60,  0
data 102,102,102,102,102, 60, 24,  0, 99, 99, 99,107,127,119, 99,  0
data 102,102, 60, 24, 60,102,102,  0,102,102,102, 60, 24, 24, 24,  0
data 126,  6, 12, 24, 48, 96,126,  0, 24, 24, 24,255,255, 24, 24, 24
data 192,192, 48, 48,192,192, 48, 48, 24, 24, 24, 24, 24, 24, 24, 24
data  51, 51,204,204, 51, 51,204,204, 51,153,204,102, 51,153,204,102
data   0,  0,  0,  0,  0,  0,  0,  0,240,240,240,240,240,240,240,240
data   0,  0,  0,  0,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0
data   0,  0,  0,  0,  0,  0,  0,255,192,192,192,192,192,192,192,192
data 204,204, 51, 51,204,204, 51, 51,  3,  3,  3,  3,  3,  3,  3,  3
data   0,  0,  0,  0,204,204, 51, 51,204,153, 51,102,204,153, 51,102
data   3,  3,  3,  3,  3,  3,  3,  3, 24, 24, 24, 31, 31, 24, 24, 24
data   0,  0,  0,  0, 15, 15, 15, 15, 24, 24, 24, 31, 31,  0,  0,  0
data   0,  0,  0,248,248, 24, 24, 24,  0,  0,  0,  0,  0,  0,255,255
data   0,  0,  0, 31, 31, 24, 24, 24, 24, 24, 24,255,255,  0,  0,  0
data   0,  0,  0,255,255, 24, 24, 24, 24, 24, 24,248,248, 24, 24, 24
data 192,192,192,192,192,192,192,192,224,224,224,224,224,224,224,224
data   7,  7,  7,  7,  7,  7,  7,  7,255,255,  0,  0,  0,  0,  0,  0
data 255,255,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,255,255
data   1,  3,  6,108,120,112, 96,  0,  0,  0,  0,  0,240,240,240,240
data  15, 15, 15, 15,  0,  0,  0,  0, 24, 24, 24,248,248,  0,  0,  0
data 240,240,240,240,  0,  0,  0,  0,240,240,240,240, 15, 15, 15, 15
data 195,153,145,145,159,153,195,255,255,255,195,249,193,153,193,255
data 255,159,159,131,153,153,131,255,255,255,195,159,159,159,195,255
data 255,249,249,193,153,153,193,255,255,255,195,153,129,159,195,255
data 255,241,231,193,231,231,231,255,255,255,193,153,153,193,249,131
data 255,159,159,131,153,153,153,255,255,231,255,199,231,231,195,255
data 255,249,255,249,249,249,249,195,255,159,159,147,135,147,153,255
data 255,199,231,231,231,231,195,255,255,255,153,128,128,148,156,255
data 255,255,131,153,153,153,153,255,255,255,195,153,153,153,195,255
data 255,255,131,153,153,131,159,159,255,255,193,153,153,193,249,249
data 255,255,131,153,159,159,159,255,255,255,193,159,195,249,131,255
data 255,231,129,231,231,231,241,255,255,255,153,153,153,153,193,255
data 255,255,153,153,153,195,231,255,255,255,156,148,128,193,201,255
data 255,255,153,195,231,195,153,255,255,255,153,153,153,193,243,135
data 255,255,129,243,231,207,129,255,195,207,207,207,207,207,195,255
data 243,237,207,131,207,157,  3,255,195,243,243,243,243,243,195,255
data 255,231,195,129,231,231,231,231,255,239,207,128,128,207,239,255
data 255,255,255,255,255,255,255,255,231,231,231,231,255,255,231,255
data 153,153,153,255,255,255,255,255,153,153,  0,153,  0,153,153,255
data 231,193,159,195,249,131,231,255,157,153,243,231,207,153,185,255
data 195,153,195,199,152,153,192,255,249,243,231,255,255,255,255,255
data 243,231,207,207,207,231,243,255,207,231,243,243,243,231,207,255
data 255,153,195,  0,195,153,255,255,255,231,231,129,231,231,255,255
data 255,255,255,255,255,231,231,207,255,255,255,129,255,255,255,255
data 255,255,255,255,255,231,231,255,255,252,249,243,231,207,159,255
data 195,153,145,137,153,153,195,255,231,231,199,231,231,231,129,255
data 195,153,249,243,207,159,129,255,195,153,249,227,249,153,195,255
data 249,241,225,153,128,249,249,255,129,159,131,249,249,153,195,255
data 195,153,159,131,153,153,195,255,129,153,243,231,231,231,231,255
data 195,153,153,195,153,153,195,255,195,153,153,193,249,153,195,255
data 255,255,231,255,255,231,255,255,255,255,231,255,255,231,231,207
data 241,231,207,159,207,231,241,255,255,255,129,255,129,255,255,255
data 143,231,243,249,243,231,143,255,195,153,249,243,231,255,231,255
data 255,255,255,  0,  0,255,255,255,231,195,153,129,153,153,153,255
data 131,153,153,131,153,153,131,255,195,153,159,159,159,153,195,255
data 135,147,153,153,153,147,135,255,129,159,159,135,159,159,129,255
data 129,159,159,135,159,159,159,255,195,153,159,145,153,153,195,255
data 153,153,153,129,153,153,153,255,195,231,231,231,231,231,195,255
data 225,243,243,243,243,147,199,255,153,147,135,143,135,147,153,255
data 159,159,159,159,159,159,129,255,156,136,128,148,156,156,156,255
data 153,137,129,129,145,153,153,255,195,153,153,153,153,153,195,255
data 131,153,153,131,159,159,159,255,195,153,153,153,153,195,241,255
data 131,153,153,131,135,147,153,255,195,153,159,195,249,153,195,255
data 129,231,231,231,231,231,231,255,153,153,153,153,153,153,195,255
data 153,153,153,153,153,195,231,255,156,156,156,148,128,136,156,255
data 153,153,195,231,195,153,153,255,153,153,153,195,231,231,231,255
data 129,249,243,231,207,159,129,255,231,231,231,  0, 0, 231,231,231
data  63, 63,207,207, 63, 63,207,207,231,231,231,231,231,231,231,231
data 204,204, 51, 51,204,204,51,  51,204,102, 51,153,204,102, 51,153
data 255,255,255,255,255,255,255,255, 15, 15, 15, 15, 15, 15, 15, 15
data 255,255,255,255,  0,  0,  0,  0,  0,255,255,255,255,255,255,255
data 255,255,255,255,255,255,255,  0, 63, 63, 63, 63, 63, 63, 63, 63
data  51, 51,204,204, 51, 51,204,204,252,252,252,252,252,252,252,252
data 255,255,255,255, 51, 51,204,204, 51,102,204,153, 51,102,204,153
data 252,252,252,252,252,252,252,252,231,231,231,224,224,231,231,231
data 255,255,255,255,240,240,240,240,231,231,231,224,224,255,255,255
data 255,255,255,  7,  7,231,231,231,255,255,255,255,255,255,  0,  0
data 255,255,255,224,224,231,231,231,231,231,231,  0,  0,255,255,255
data 255,255,255,  0,  0,231,231,231,231,231,231,  7,  7,231,231,231
data  63, 63, 63, 63, 63, 63, 63, 63, 31, 31, 31, 31, 31, 31, 31, 31
data 248,248,248,248,248,248,248,248,  0,  0,255,255,255,255,255,255
data   0,  0,  0,255,255,255,255,255,255,255,255,255,255,  0,  0,  0
data 254,252,249,147,135,143,159,255,255,255,255,255, 15, 15, 15, 15
data 240,240,240,240,255,255,255,255,231,231,231,  7,  7,255,255,255
data  15, 15, 15, 15, 255,255,255,255,15, 15, 15, 15,240,240,240,240

enum FB_KEYS
  fb_bspace =   8
  fb_tab    =   9
  fb_f2     = 316
  fb_f3     = 317
  fb_pos1   = 327
  fb_up     = 328
  fb_left   = 331
  fb_right  = 333
  fb_down   = 336
  fb_del    = 339
end enum
enum C64_KEYS
  c64_down   =  17
  c64_pos1   =  19
  c64_bspace =  20
  c64_right  =  29
  c64_up     = 145
  c64_left   = 157
  c64_del    = 148
end enum


dim shared as integer flag

function InterruptService(cpu as CPU6510 ptr) as integer
  static as string s
  dim as integer key
  dim as integer IRQTicks
  ' return if any interrupt are active
  if cpu->F.i=1 then return 0

  dim as integer nChars=cpu->mem->ReadUbyte(&H00C6)
  if nChars<10 then
    dim as string  strkey=inkey
    key   =len(strkey)
    if key then
      key=strkey[key-1]+(key-1)*256
      select case key
      case 27 : end
      case 65 to  90:key+=32:s=s & chr(key):dprint(s)
      case 97 to 122:key-=32:s=s & chr(key):dprint(s)
      case fb_f2 ' save
      locate 1,1: print space(48)
      locate 1,1: input "save filename:";strKey
      if len(strKey) then
        key=freefile
        if open(strKey for binary access write as #key)=0 then
          dim as ubyte   u8
          dim as integer nBytes=cpu->mem->ReadUShort(&H02D)
          nBytes-=2048
          put #key,,nBytes
          for i as integer=0 to nBytes-1
            u8=cpu->mem->ReadUByte(2048+i)
            put #key,,u8
          next
          close #key
        else
          locate 1,1: print space(48)
          locate 1,1: print "can't create: " & strKey
          beep:sleep
        end if
      end if
      line (0,0)-(319+8*8,7),3,bf
      key=0
      case fb_f3 ' load
      locate 1,1: print space(48)
      locate 1,1: input "load filename:";strKey
      if len(strKey) then
        key=freefile
        if open(strKey for binary access read as #key)=0 then
          dim as ubyte   u8
          dim as integer nBytes
          get #key,,nBytes
          for i as integer=0 to nBytes-1
            get #key,,u8
            cpu->mem->WriteUByte(2048+i,u8)
          next
          close #key
          nBytes+=2048
          cpu->mem->WriteUShort(&H02D,nBytes)
          cpu->PC=&HA52A
        else
          locate 1,1: print space(48)
          locate 1,1: print "can't open: " & strKey
          beep:sleep
        end if
      end if
      line (0,0)-(319+8*8,7),3,bf
      key=0
      case fb_pos1  :key=c64_pos1
      case fb_bspace:key=c64_bspace
      case fb_left  :key=c64_left
      case fb_up    :key=c64_up
      case fb_right :key=c64_right
      case fb_down  :key=c64_down
      end select
      if key then
        cpu->mem->WriteUbyte(&H0277+nChars,key)
        cpu->mem->WriteUbyte(&H00C6,nChars+1)
      end if
    end if
  end if
  cpu->push(cpu->ph)
  cpu->push(cpu->pl)
  cpu->push(cpu->p )
  cpu->ph=cpu->mem->ReadUbyte(&HFFFE)
  cpu->pl=cpu->mem->ReadUbyte(&HFFFF)
  cpu->F.b=0:cpu->F.i=1
  while cpu->code.code<>&H40 ' RTI
    cpu->Tick:IRQTicks+=1
  wend
  cpu->P =cpu->pull()
  cpu->pl=cpu->pull()
  cpu->ph=cpu->pull()
  if key=13 then
    flag=1
  else
    flag=0
  end if
  return IRQTicks
end function

'
' main
'
dim as OldSchool compi
dim as integer ticks

while 1
  Ticks+=1
  if flag=1 then
    compi.cpu->Tick Ticks
  else
    compi.cpu->Tick
  end if
  if Ticks mod 24000=0 then
    Ticks+=InterruptService(compi.cpu)
    sleep(10,1)
  end if
wend
end
