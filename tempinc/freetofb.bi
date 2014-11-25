'By cha0s @ freeBASIC forum
Declare sub palette_using_dib( byval dib as FIBITMAP ptr )
Declare function freeimage_to_fbgfx( byval dib as FIBITMAP Ptr, byval set_palette as integer = 0 ) as fb.IMAGE Ptr
Declare Function freeimage_load_fb( byref filename as string, byval set_palette as integer = 0 ) as fb.IMAGE Ptr

sub palette_using_dib( byval dib as FIBITMAP ptr )
 
  dim as RGBQUAD ptr pal = FreeImage_GetPalette( DIB )
 
  for i as integer = 0 to 255

    with pal[i]
     
      palette i, cubyte( .rgbRed ), cubyte( .rgbGreen ), cubyte( .rgbBlue )
   
    end with
 
  next

end sub

function freeimage_to_fbgfx( byval dib as FIBITMAP Ptr, byval set_palette as integer = 0 ) as fb.IMAGE ptr
       
        dim as fb.IMAGE ptr res = imagecreate( FreeImage_GetWidth( dib ), FreeImage_GetHeight( dib ) )
       
        dim as any ptr to_pix = res + 1
        dim as integer bpp = FreeImage_GetBPP( dib )
        select case as const bpp
        case 24
                bpp = 32
        case 15
                bpp = 16
        end select
        bpp \= 8
        var p = res->pitch, pal = FreeImage_GetPalette( dib ), rb = res->bpp
       
        FreeImage_FlipVertical( dib )
        if( pal ) then
                if( rb = 1 ) then
                        for h as integer = 0 to res->height-1
                                for w as integer = 0 to res->width-1
                                        FreeImage_GetPixelIndex( dib, w, h, to_pix + h * p + w * rb )
                                next
                        next
                        if( set_palette ) then
                                palette_using_dib( dib )
                        end if
                else
                        for h as integer = 0 to res->height-1
                                for w as integer = 0 to res->width-1
                                        dim as ubyte u
                                        FreeImage_GetPixelIndex( dib, w, h, @u )
                                        memcpy( to_pix + h * p + w * rb, pal + u, rb )
                                next
                        next
                end if
        else
                if( rb = 1 ) then
                        var new_d = FreeImage_ColorQuantizeEx( dib )
                        FreeImage_FlipVertical( new_d )
                        function = freeimage_to_fbgfx( new_d )
                        if( set_palette ) then
                                palette_using_dib( new_d )
                        end if
                        FreeImage_Unload( new_d )
                        exit function
                end if
                for h as integer = 0 to res->height-1
                        for w as integer = 0 to res->width-1
                                FreeImage_GetPixelColor( dib, w, h, to_pix + h * p + w * rb )
                        next
                next
        end if
        FreeImage_FlipVertical( dib )
       
        function = res
       
end function

function freeimage_load_fb( byref filename as string, byval set_palette as integer = 0 ) as fb.IMAGE ptr
       
        var dib = FreeImage_Load( FreeImage_GetFileType( filename ), filename )
       
        function = freeimage_to_fbgfx( dib, set_palette )
       
        FreeImage_Unload( dib )
       
end function
