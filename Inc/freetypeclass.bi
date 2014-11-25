'
' True type font class. Simple api for the freetype libray.
'
' By Thorham.
' Based on the freetype example from the FreeBasic examples.
'
#Include "freetype2/freetype.bi"

Type truetype
        Dim errormsg As ft_error
        Dim library As ft_library

        Dim face As ft_face
        Dim string_width As Integer
        Dim render_mode As Integer

        Dim scr_width As Integer
        Dim scr_height As Integer
        Dim bmp_dat As Uinteger

        Dim font_size As Integer

        Dim bitmapft As ft_bitmap
        Dim bitmapptr As Ubyte Ptr
        Dim destptr As Uinteger Ptr

        Dim bitmappitch As Integer

        Dim src_rb As Uinteger
        Dim src_g As Uinteger
        Dim src_color As Uinteger

        Dim dst_rb As Uinteger
        Dim dst_g As Uinteger
        Dim dst_color As Uinteger

        'extended
        Dim As Uinteger rb,g

        'methods
        Declare Function init As ft_error
        Declare Function get_font(fontname As String) As Integer
        Declare Sub set_render_mode(rm As Integer)
        Declare Sub set_screen_size(scr_width As Integer, scr_height As Integer)
        Declare Function set_size(size As Integer) As Integer
        Declare Sub set_color(clr As Uinteger)
        Declare Sub set_back_color(clr As Uinteger)
        Declare Function print_char(x As Integer, y As Integer, char As ft_ulong) As Integer
        Declare Function get_text_width(txt As String) As Integer
        Declare Function print_text(x As Integer, y As Integer, txt As String) As Integer
End Type

Function truetype.init As ft_error
        Return ft_init_freetype(@library)
End Function

Function truetype.get_font(FontName As String) As Integer
        errormsg = ft_new_face(library, fontname, 0, @face )

        If errormsg Then
                Return 0
        Else
                Return 1
        Endif

End Function
'
' Render mode:
'
' FT_RENDER_MODE_MONO for 1 bit per pixel rendering.
' FT_RENDER_MODE_NORMAL for anti-aliased rendering.
'
Sub truetype.set_render_mode(rm As Integer)
        render_mode=rm
End Sub

Sub truetype.set_screen_size(w As Integer, h As Integer)
        scr_width=w
        scr_height=h
End Sub

Function truetype.set_size(size As Integer) As Integer
        errormsg=ft_set_pixel_sizes(face, size, size)

        If errormsg Then
                font_size=0
                Return 0
        Else
                font_size=size
                Return 1
        Endif

End Function

Sub truetype.set_color(clr As Uinteger)
        src_color=clr
        src_rb=clr And &h00ff00ff
        src_g=clr And &h0000ff00
End Sub

Sub truetype.set_back_color(clr As Uinteger)
        dst_color=clr
        dst_rb=clr And &h00ff00ff
        dst_g=clr And &h0000ff00
End Sub

Function truetype.get_text_width(txt As String) As Integer
        string_width=0

        For t As Integer=0 To Len(txt)-1
                errormsg = ft_load_char(face, txt[t], FT_LOAD_DEFAULT)
                If errormsg Then Return 0

                string_width=string_width+face->glyph->advance.x Shr 6
        Next

        Return string_width
End Function

Function truetype.print_char(x As Integer, y As Integer, char As ft_ulong) As Integer
        errormsg = ft_load_char(face, char, FT_LOAD_DEFAULT):If errormsg Then Return 0
        errormsg = ft_render_glyph(face->glyph, render_mode):If errormsg Then Return 0

        ' Check clipping
        If (x + face->glyph->bitmap_left + face->glyph->bitmap.width) > scr_width Then Return 0
        If (y - face->glyph->bitmap_top + face->glyph->bitmap.rows) > scr_height Then Return 0
        If (x + face->glyph->bitmap_left) < 0 Then Return 0
        If (y - face->glyph->bitmap_top) < 0 Then Return 0

        x=x + face->glyph->bitmap_left
        y=y - face->glyph->bitmap_top

        bitmapft = face->glyph->bitmap
        bitmapptr = bitmapft.buffer
        destptr = Cast(Uinteger Ptr, ScreenPtr) + (y * scr_width) + x

        If render_mode=FT_RENDER_MODE_NORMAL Then        'Render anti aliased glyph.

                bitmappitch = scr_width - bitmapft.width

                For yy As Integer=0 To bitmapft.rows-1
                        For xx As Integer=0 To bitmapft.width-1

                                If *bitmapptr<>0 Then
                                        rb = ((src_rb - dst_rb) * *bitmapptr) Shr 8
                                        g  = ((src_g - dst_g) * *bitmapptr) Shr 8

                                        *destptr = ((dst_rb + rb) And &h00ff00ff) Or ((dst_g + g) And &h0000ff00)
                                Endif

                                destptr += 1
                                bitmapptr += 1
                        Next

                        destptr += bitmappitch
                Next

        Else        'Render one bit per pixel glyph.
                bitmappitch = scr_width - bitmapft.pitch Shl 3

                For yy As Integer=0 To bitmapft.rows-1
                        For xx As Integer=0 To bitmapft.pitch-1
                                bmp_dat=*bitmapptr

                                For b As Integer=0 To 7
                                        If (bmp_dat And 128)=128 Then *destptr=src_color

                                        bmp_dat=bmp_dat Shl 1
                                        destptr += 1
                                Next

                                bitmapptr += 1
                        Next

                        destptr = destptr+bitmappitch
                Next

        End If

        Return face->glyph->advance.x Shr 6
End Function

Function truetype.print_text(x As Integer, y As Integer, txt As String) As Integer
        Dim As Integer xx
        For t As Integer=0 To Len(txt)-1
                xx=xx+print_char(x+xx,y,txt[t])
        Next

        Return xx
End Function

