' by D.J.Peters (Joshy)
' an put, scale, rotate hackfor the new ImageHeader format.
' MultiPut [destination],[xmidpos],[ymidpos],source,[xScale],[yScale],[Trans]



#define UseRad 'if not then Rotate are in degres

Sub MultiPut(Byval lpTarget As Any Ptr= 0, _
             Byval xMidPos  As Integer= 0, _
             Byval yMidPos  As Integer= 0, _
             Byval lpSource As Any Ptr   , _
             Byval xScale   As Single = 1, _
             Byval yScale   As Single = 1, _
             Byval Rotates   As Single = 0, _
             Byval Trans    As Integer= 0)

  If (screenptr=0) Or (lpSource=0) Then Exit Sub

  If xScale < 0.001 Then xScale=0.001
  If yScale < 0.001 Then yScale=0.001

  Dim As Integer MustLock,MustRotate

  If lpTarget= 0 Then MustLock  =1
  If Rotates  <>0 Then MustRotate=1

  Dim As Integer  TargetWidth,TargetHeight,TargetBytes,TargetPitch
  If MustLock Then
    ScreenInfo    _
    TargetWidth , _
    TargetHeight, _
    TargetBytes ,,_
    TargetPitch
    TargetBytes Shr=3

    lpTarget=ScreenPtr
  Else
    TargetBytes  = cptr(Uinteger Ptr,lpTarget)[1]
    TargetWidth  = cptr(Uinteger Ptr,lpTarget)[2]
    TargetHeight = cptr(Uinteger Ptr,lpTarget)[3]
    TargetPitch  = cptr(Uinteger Ptr,lpTarget)[4]
    lpTarget    += 32
  End If
  If (TargetWidth<4) Or (TargetHeight<4) Then Exit Sub

  Dim As Integer   SourceWidth,SourceHeight,SourceBytes,SourcePitch
  If cptr(Integer Ptr,lpSource)[0] = 7 Then
    SourceBytes  = cptr(Uinteger Ptr,lpSource)[1]
    SourceWidth  = cptr(Uinteger Ptr,lpSource)[2]
    SourceHeight = cptr(Uinteger Ptr,lpSource)[3]
    SourcePitch  = cptr(Uinteger Ptr,lpSource)[4]
    lpSource    += 32
  Else
    SourceBytes  = 1
    SourceWidth  = cptr(Ushort Ptr,lpSource)[0] Shr 3
    SourceHeight = cptr(Ushort Ptr,lpSource)[1]
    SourcePitch  = SourceWidth
    lpSource    += 4
  End If
#if 0
  ? TargetWidth & "x" & TargetHeight & "x" & TargetBytes,TargetPitch
  ? SourceWidth & "x" & SourceHeight & "x" & SourceBytes,SourcePitch
  ? MustLock,Trans
  Sleep:End
#endif

  If (SourceWidth<2) Or (SourceHeight<2) Then Exit Sub
  If (TargetBytes<>SourceBytes) Then Exit Sub

#define xs 0 'screen
#define ys 1
#define xt 2 'texture
#define yt 3
  Dim As Single Points(3,3)
  points(0,xs)=-SourceWidth/2 * xScale
  points(1,xs)= SourceWidth/2 * xScale
  points(2,xs)= points(1,xs)
  points(3,xs)= points(0,xs)

  points(0,ys)=-SourceHeight/2 * yScale
  points(1,ys)= points(0,ys)
  points(2,ys)= SourceHeight/2 * yScale
  points(3,ys)= points(2,ys)

  points(1,xt)= SourceWidth-1
  points(2,xt)= points(1,xt)
  points(2,yt)= SourceHeight-1
  points(3,yt)= points(2,yt)

  Dim As Uinteger i
  Dim As Single x,y
  If MustRotate Then
    #ifndef UseRad
    Rotate*=0.017453292 'degre 2 rad
    #endif
    While Rotates< 0        :rotates+=6.2831853:Wend
    While Rotates>=6.2831853:rotates-=6.2831853:Wend
    For i=0 To 3
      x=points(i,xs)*Cos(Rotates) - points(i,ys)*Sin(Rotates)
      y=points(i,xs)*Sin(Rotates) + points(i,ys)*Cos(Rotates)
      points(i,xs)=x:points(i,ys)=y
    Next
  End If

  Dim As Integer yStart,yEnd,xStart,xEnd
  yStart=100000:yEnd=-yStart:xStart=yStart:xEnd=yEnd

#define LI 0   'LeftIndex
#define RI 1   'RightIndex
#define  IND 0 'Index
#define NIND 1 'NextIndex
  Dim As Integer CNS(1,1) 'Counters

  For i=0 To 3
    points(i,xs)=Int(points(i,xs)+xMidPos)
    points(i,ys)=Int(points(i,ys)+yMidPos)
    If points(i,ys)<yStart Then yStart=points(i,ys):CNS(LI,IND)=i
    If points(i,ys)>yEnd   Then yEnd  =points(i,ys)
    If points(i,xs)<xStart Then xStart=points(i,xs)
    If points(i,xs)>xEnd   Then xEnd  =points(i,xs)
  Next
  If yStart =yEnd         Then Exit Sub
  If yStart>=TargetHeight Then Exit Sub
  If yEnd   <0            Then Exit Sub
  If xStart = xEnd        Then Exit Sub
  If xStart>=TargetWidth  Then Exit Sub
  If xEnd   <0            Then Exit Sub

  Dim As Ubyte    Ptr t1,s1
  Dim As Ushort   Ptr t2,s2
  Dim As Uinteger Ptr t4,s4


#define ADD 0
#define CMP 1
#define SET 2
  Dim As Integer ACS(1,2) 'add compare and set
  ACS(LI,ADD)=-1:ACS(LI,CMP)=-1:ACS(LI,SET)=3
  ACS(RI,ADD)= 1:ACS(RI,CMP)= 4:ACS(RI,SET)=0

#define EX  0
#define EU  1
#define EV  2
#define EXS 3
#define EUS 4
#define EVS 5
  Dim As Single E(2,6),S(6),Length,uSlope,vSlope
  Dim As Integer U,UV,UA,UN,V,VV,VA,VN

  ' share the same highest point
  CNS(RI,IND)=CNS(LI,IND)
  If MustLock Then ScreenLock
  ' loop from Top to Bottom
  While yStart<yEnd
    'Scan Left and Right sides together
    For i=LI To RI
      ' bad to read but fast and short ;-)
      If yStart=points(CNS(i,IND),ys) Then
        CNS(i,NIND)=CNS(i,IND)+ACS(i,Add)
        If CNS(i,NIND)=ACS(i,CMP) Then CNS(i,NIND)=ACS(i,SET)
        While points(CNS(i,IND),ys) = points(CNS(i,NIND),ys)
          CNS(i, IND)=CNS(i,NIND)
          CNS(i,NIND)=CNS(i, IND)+ACS(i,Add)
          If CNS(i,NIND)=ACS(i,CMP) Then CNS(i,NIND)=ACS(i,SET)
        Wend
        E(i,EX) = points(CNS(i, IND),xs)
        E(i,EU) = points(CNS(i, IND),xt)
        E(i,EV) = points(CNS(i, IND),yt)
        Length  = points(CNS(i,NIND),ys)
        Length -= points(CNS(i, IND),ys)
        If Length <> 0.0 Then
          E(i,EXS) = points(CNS(i, NIND),xs)-E(i,EX):E(i,EXS)/=Length
          E(i,EUS) = points(CNS(i, NIND),xt)-E(i,EU):E(i,EUS)/=Length
          E(i,EVS) = points(CNS(i, NIND),yt)-E(i,EV):E(i,EVS)/=Length
        End If
        CNS(i,IND)=CNS(i,NIND)
      End If
    Next

    If (yStart<0)                              Then Goto SkipScanLine
    xStart=E(LI,EX)+0.5:If xStart>=TargetWidth Then Goto SkipScanLine
    xEnd  =E(RI,EX)-0.5:If xEnd  < 0           Then Goto SkipScanLine
    If (xStart=xEnd)                           Then Goto SkipScanLine
    'if xEnd  <xStart                           then goto SkipScanLine
    Length=xEnd-xStart
    uSlope=E(RI,EU)-E(LI,EU):uSlope/=Length
    vSlope=E(RI,EV)-E(LI,EV):vSlope/=Length
    If xstart<0 Then
      Length=Abs(xStart)
      U=Int(E(LI,EU)+uSlope*Length)
      V=Int(E(LI,EV)+vSlope*Length)
      xStart = 0
    Else
      U=Int(E(LI,EU)):V=Int(E(LI,EV))
    End If
    If xEnd>=TargetWidth Then xEnd=TargetWidth-1
    UV=Int(uSlope):UA=(uSlope-UV)*100000:UN=0
    VV=Int(vSlope):VA=(vSlope-VV)*100000:VN=0
    xEnd-=xStart
    Select Case TargetBytes
      Case 1
        t1=cptr(Ubyte Ptr,lpTarget)
        t1+=yStart*TargetPitch+xStart:xStart=0
        If Trans=0 Then
          While xStart<xEnd
            s1=lpSource+V*SourcePitch+U
            *t1=*s1
            U+=UV:UN+=UA:If UN>=100000 Then U+=1:UN-=100000
            V+=VV:VN+=VA:If VN>=100000 Then V+=1:VN-=100000
            If u<0 Then u=0
            If v<0 Then v=0
            xStart+=1:t1+=1
          Wend
        Else
          While xStart<xEnd
            s1=lpSource+V*SourcePitch+U
            If *s1 Then *t1=*s1
            U+=UV:UN+=UA:If UN>=100000 Then U+=1:UN-=100000
            V+=VV:VN+=VA:If VN>=100000 Then V+=1:VN-=100000
            If u<0 Then u=0
            If v<0 Then v=0
            xStart+=1:t1+=1
          Wend
        End If
      Case 2
        t2=cptr(Short Ptr,lpTarget)
        t2+=yStart*(TargetPitch Shr 1)+xStart:xStart=0
        If Trans=0 Then
          While xStart<xEnd
            s2=cptr(Short Ptr,lpSource)+V*(SourcePitch Shr 1)+U
            *t2=*s2
            U+=UV:UN+=UA:If UN>=100000 Then U+=1:UN-=100000
            V+=VV:VN+=VA:If VN>=100000 Then V+=1:VN-=100000
            If u<0 Then u=0
            If v<0 Then v=0
            xStart+=1:t2+=1
          Wend
        Else
          While xStart<xEnd
            s2=cptr(Short Ptr,lpSource)+V*(SourcePitch Shr 1)+U
            If *s2<>&HF81F Then *t2=*s2
            U+=UV:UN+=UA:If UN>=100000 Then U+=1:UN-=100000
            V+=VV:VN+=VA:If VN>=100000 Then V+=1:VN-=100000
            If u<0 Then u=0
            If v<0 Then v=0
            xStart+=1:t2+=1
          Wend
        End If
      Case 4
        t4=cptr(Integer Ptr,lpTarget)+yStart*(TargetPitch Shr 2)+xStart:xStart=0
        If Trans=0 Then
          While xStart<xEnd
            s4=cptr(Integer Ptr,lpSource)+V*(SourcePitch Shr 2)+U
            *t4=*s4
            U+=UV:UN+=UA:If UN>=100000 Then U+=1:UN-=100000
            V+=VV:VN+=VA:If VN>=100000 Then V+=1:VN-=100000
            If u<0 Then u=0
            If v<0 Then v=0
            xStart+=1:t4+=1
          Wend
        Else
          While xStart<xEnd
            's4=cptr(Integer Ptr,lpSource):s4+=V*(SourcePitch shr 2):s4+=U
            s4=cptr(Integer Ptr,lpSource)+V*(SourcePitch Shr 2)+U
            If *s4<>&HFFFF00FF Then *t4=*s4
            U+=UV:UN+=UA:If UN>=100000 Then U+=1:UN-=100000
            V+=VV:VN+=VA:If VN>=100000 Then V+=1:VN-=100000
            If u<0 Then u=0
            If v<0 Then v=0
            xStart+=1:t4+=1
          Wend
        End If
    End Select

SkipScanLine:
    E(LI,EX)+=E(LI,EXS):E(LI,EU)+=E(LI,EUS):E(LI,EV)+=E(LI,EVS)
    E(RI,EX)+=E(RI,EXS):E(RI,EU)+=E(RI,EUS):E(RI,EV)+=E(RI,EVS)
    yStart+=1:If yStart=TargetHeight Then yStart=yEnd 'exit loop
  Wend
If MustLock Then ScreenUnlock
End Sub

