[BITS 16]
[ORG 0200h]
[map]

main:
  ; set up our registers and data segment for our memory location
  cli
  xor bx, bx
  mov ax, bx
  mov cx, bx
  mov dx, bx
  ; setup stack pointer
  mov bx, 1000h
  mov ss, bx
  mov bx, 0FFFFh
  mov sp, bx
  ; setup data segment for this code block at origin 1000h
  mov bx, 1000h
  mov ds, bx
  sti

  ; Setup initial vidmode
  call InitTextMode

RunOS:
  ; lea  si, [loadmsg]              ; load address of initial boot message
  ; call WriteString
  ; lea  si, [vgatestpromptmsg]     ; Load address of vgatest message into si
  ; call WriteString                ; print the string
  ; call KeypressWaitLoop

  call VGAMode13FontTest
  ; call VGAMode13Test
  ; call VGAMode12Test

  call HaltAndCatchFire

WriteString:
  mov   ah, 0Ah                   ; set vga textmode attrs
  mov   bx, [vgatmodebuf]
  mov   es, bx

  .loop:
    lodsb                           ; load byte at ds:si into al (advancing si)

    or     al, al                   ; test if character is 0 (end)
    jz     .done                    ; if 0 reached, then routine is done

    cmp    al, 0Ah
    jz     WriteNewline

    cmp    al, 0Dh
    jz     WriteCarriageReturn

    mov    [es:di], ax              ; write word to vga text buffer
    times 2 inc di                  ; 2 bytes need to be written to the vga text buffer per character
    jmp .loop                       ; Repeat for next character.

  .done:
    call SleepSecond
    ret                             ; Return to calling subroutine

WriteNewline:
  add di, 80*2
  jmp WriteString.loop

WriteCarriageReturn:
  push ax                         ; save registers
  push bx
  push dx

  mov ax, di                      ; divisor low word
  mov dx, 0                       ; divisor high word
  mov bx, 80                      ; dividend
  div bx

  mov bx, 80                      ; number of text columns
  mul bx                          ; multiply the number rows written by the column count to get the buffer position

  mov di, ax

  pop dx
  pop bx                          ; restore registers
  pop ax

  call UpdateCursor
  jmp WriteString.loop

Reboot:
  lea  si, [rebootmsg]            ; Load address of reboot message into si
  call WriteString                ; print the string
  db   0xEA                       ; unassembled machine language to jump to FFFF:0000 (reboot)
  dw   0x0000
  dw   0xFFFF

 SleepSecond:
  push ax
  push bx

  mov ax, 0Fh
  mov bx, 04240h                 ; 1,000,000 us = 1 s
  call Sleep

  pop bx
  pop ax
  ret
  
; bios wait service expects 16bit wait period setup in cx (high byte) and dx (low byte) registers
; actual value is concatenation of high + low in hex to set wait period in microseconds (1,000,000us = 1s)
; for example 0F4240h = 1second or 1million microseconds 01E8480 = 2 seconds
; https://dos4gw.org/INT_15H_86H_Wait
Sleep:
  push ax
  push bx
  push cx
  push dx

  mov     cx, ax                  ; high byte
  mov     dx, bx                  ; low byte  

  xor     ax, ax                  ; zero out ax
  mov     ah, 86h                 ; wait subfunction
  int     15h                     ; trigger bios service
  
  pop dx
  pop cx
  pop bx
  pop ax  
  ret

KeypressWaitLoop:
  push ax
  xor  ax, ax                     ; subfuction 0
  int  16h                        ; call bios to wait for key
  pop ax
  ret

  ; VGA CRTC registers 14 and 15 contain the MSB and LSB of the cursor position, relative to B8000h or B0000h, in units of characters.
UpdateCursor:
  ; save registers on the stack
  pusha

  ; move cursor column index into bx(bl actually), di = current memory pos of vga text buffer, with 2 bytes per char written, 
  ; to get cursor pos memory pos needs to be divided by 2
  mov ax, di                    ; current textmode memory pos as dividend
  mov dx, 0
  mov bx, 2                     ; divisor
  div bx
  mov bx, ax                    ; ax has the 16 bit quotient result

  mov dx, [crtcreg]             ; write 16bit crtc base io port address to dx (index register)
  mov al, 0Fh                   ; set address index 15 (cursor pos low byte)
  out dx, al                    ; write to io port

  inc dl                        ; vga data register is crtc port + 1
  mov al, bl                    ; send low byte of cursor position to crtc data register
  out dx, al                    ; write to io port

  dec dl                        ; revert back to crtc base io port address
  mov al, 0x0E                  ; select index 10 (cursor pos high byte)
  out dx, al                    ; write to io port

  inc dl                        ; select data port address
  mov al, bh                    ; send high byte of cursor position to crtc data register
  out dx, al                    ; write to io port

  ; restore registers
  popa
  
  ret
  
VGAMode12FontTest:
  %define CHAR_WIDTH  8
  %define CHAR_HEIGHT 16

  call SleepSecond
  push bp
  mov bp, sp              ; save stack frame

  push 12h                ; first arg vga mode 12h  (640x480 16 color planar)
  call InitGraphicsMode
  
  mov di, vgafont
  call LoadVGABiosFont

  mov es, [vgagmodebuf]
  mov di, 0
  mov si, vgafont
  mov bx, 1 ; initial color

 .ol1:
    mov cx, 80    ; num chars per row (8x16) i.e. 640/8
    .il1:
      cmp si, vgafont + 4096 ; font bitmap is 256 chars of 8x16
      jge .llend

      push cx
      push si       ; 1st arg, index of glyph 8x8 bitmap
      push di       ; 2nd arg, mem pos of top left pixel
      push bx       ; 3rd arg colour
      call WriteGlyph12h
      
      pop bx
      pop ax
      mov di, ax
      pop ax
      mov si, ax
      pop cx
    .endil1:

    add si, 16  ; move to next char in charset map, 16 bytes per
    inc di      ; move to next display column (remember it's 8 pixels per byte)
    inc bx      ; cycle to next  color
    
    .ifColorIsBlack:
    cmp bx, 10h ; black is kinda useless...
    jnz .endIfColorIsBlack
    mov bx, 1
    .endIfColorIsBlack:

    dec cx
    jnz .il1

    ; new line
    add di, 1280 ; return to far left edge + descend 10 pixels down (640 / 8 pixels per byte * 10 rows)
    
    jmp .ol1
  
  .llend:
    jmp HaltAndCatchFire

VGAMode13FontTest:
  push bp
  mov bp, sp              ; save stack frame

  push 13h                ; first arg vga mode 13h 
  call InitGraphicsMode

  ; load bios vga font (8x16)
  ; mov di, vgafont
  ; call LoadVGABiosFont

  mov es, [vgagmodebuf]
  mov di, 0
  mov si, font_bitmap

  .ol1:
    mov cx, 32    ; num chars per row (8x8 font in a 10x10 cell)
  
    .il1:
      cmp si, font_bitmap + 2048
      jge .llend

      push cx
      push si       ; 1st arg, index of glyph 8x8 bitmap
      push di       ; 2nd arg, mem pos of top left pixel
      push 0Ah      ; 3rd arg colour
      call WriteGlyph13h
      
      pop ax
      pop ax
      mov di, ax
      pop ax
      mov si, ax
      pop cx
    .endil1:

    add si, 8  ; move to next char in charset map
    add di, 10 ; each pixel square is 8x8 with 1 pixel padding each side
    dec cx
    jnz .il1
    ; new line
    add di, 3200 ; return to far left edge + descend 10 pixels down (for char height, plus 1 pixel of top & bottom padding)
    jmp .ol1
  
  .llend:
    jmp HaltAndCatchFire
  
WriteGlyph12h:
  push bp
  mov bp, sp      ; setup stack frame

  mov cx, 16      ; render 1 byte per loop, 16 bytes per char
  mov ax, [bp+4]  ; color
  mov di, [bp+6]  ; screen top left loc
  mov si, [bp+8]  ; glyph

  ; ax contains plane mask, when planes are chained together, this effectively sets color
  call PlaneSelect
  
  .WriteGlyphLoop:
    lodsb
    mov [es:di], al
    
    add di, 640/8   ; advance memory to next line (remember its 640px wide but each memory address has 8 pixels (1 bit per pix))
    
    dec cx
    jnz .WriteGlyphLoop
    
  pop bp
  ret

WriteGlyph13h:
  push bp
  mov bp, sp
  mov cx, 8       ; render 1 byte per loop, 8 bytes per char
  mov bx, [bp+4]  ; color
  mov di, [bp+6]  ; screen top left loc
  inc di          ; add one pixel of left padding
  add di, 320     ; add 1 pixel top padding
  mov si, [bp+8]  ; glyph

  .WriteGlyphLoop:
    lodsb           ; virtual mov al, [ds:si]; inc si
    
    test al, 80h    ; if bit set
    jz .next_1
    mov [es:di], bx ; write fill color

    .next_1:
    test al, 40h
    jz .next_2
    mov [es:di+1], bx
    
    .next_2:
    test al, 20h
    jz .next_3
    mov [es:di+2], bx
    
    .next_3:
    test al, 10h
    jz .next_4
    mov [es:di+3], bx
    
    .next_4:
    test al, 08h
    jz .next_5
    mov [es:di+4], bx
    
    .next_5:
    test al, 04h
    jz .next_6
    mov [es:di+5], bx

    .next_6:
    test al, 02h
    jz .next_7
    mov [es:di+6], bx

    .next_7:
    test al, 01h
    jz .next_8
    mov [es:di+7], bx

    .next_8:
    add di, 320

    dec cx
    jnz .WriteGlyphLoop
    
    pop bp
    ret

VGAMode13Test:
  push 13h ; 320x200 256 color linear
  call InitGraphicsMode
  mov es, [vgagmodebuf]
  mov ax, 0
  mov bx, 0

VGAMode13Refresh:
  mov ax, 0
  mov di, ax
  inc bx
  
  push ax
  push dx
  call WaitForVBlank
  pop dx
  pop ax
    
VGA13ColorFillTest:
  mov [es:di], bl
  mov [es:di + 1], bl
  times 2 inc di
  cmp di, 320*200
  jnz VGA13ColorFillTest
  jmp VGAMode13Refresh

VGAMode12Test:
  .VGAInit:
    mov ax, 12h
    push ax             ; 640x480 16 color planar
    call InitGraphicsMode
    mov ax, 00h         ; initial pixel fill colour

  .VGALoop:
    call TestVGAGraphics
    ; call SleepSecond
    jmp .VGALoop
  
  .VGAEnd:
    ret

; ax should contain pixel color
TestVGAGraphics:
  push ax
  push bx
  push cx
  push dx
  
  mov es, [vgagmodebuf]
  ; if in mode 12h, i.e. 4bit color (16 colors), 640x480 resolution we write 8 pixels, across 4 bytes per loop (color is written across the 4 bytes)
  ; therefore 640 * 480 pixels / 8 (8 pixels written per loop) 
  mov cx, (640 * 480) / 8 ; number of loops for the pixels to write
  mov dx, 0
  mov di, 0

  push ax
  push dx
  call WaitForVBlank
  pop dx
  pop ax

  .loop:
    cmp cx, di
    jz .done

    cmp ax, 10h       ; when we reach 16 we have painted all our colors, so reset back
    jz .resetcolor

    ; select plane0
    push ax
    mov ax, 1
    call PlaneSelect
    pop ax
    mov bx, ax
    and bx, 1
    xor bx, 0FFh
    inc bx
    mov [es:di], bx ; write plane 0 

    push ax
    mov ax, 2
    call PlaneSelect
    pop ax
    mov bx, ax
    shr bx, 1
    and bx, 1
    xor bx, 0FFh
    inc bx
    mov [es:di], bx ; write plane 1 

    push ax
    mov ax, 4
    call PlaneSelect
    pop ax
    mov bx, ax
    shr bx, 2
    and bx, 1
    xor bx, 0FFh
    inc bx
    mov [es:di], bx ; write plane 2 

    push ax
    mov ax, 8
    call PlaneSelect
    pop ax
    mov bx, ax
    shr bx, 3
    and bx, 1
    xor bx, 0FFh
    inc bx
    mov [es:di], bx ; write plane 3

    inc di

    ; with 16 colors over 640 pixels, as in mode 12 we render 8 pixels per loop 
    ; changing color every 5 loops will give us 40pixel colored columns
    pusha
    mov ax, di
    mov bx, 5
    div bx
    cmp dx, 0
    popa
    jnz .loop

    inc ax
    jmp .loop

  .resetcolor:
    mov ax, 0
    jmp .loop

  .done:
    pop dx
    pop cx
    pop bx
    pop ax
    ret

PlaneSelect:
  push dx
  push ax
  mov al, 02h   ; index
  mov dx, [mapmaskreg]
  out dx, al    ; map mask register + index selects plane we want to write to
  pop ax
  inc dx
  out dx, ax
  pop dx
  ret

ResetDisk:
  push ax
  push dx
  mov dl, [bootdrive]            ; select drive to read from
  xor ax, ax                      ; subfunction 0
  int 13h                         ; call interrupt 13h
  jc ResetDisk                    ; retry as error if carry set
  pop dx
  pop ax
  ret

HaltAndCatchFire:
  hlt
  jmp HaltAndCatchFire

SetVidMode:
  xor  ax, ax                     ; Rub out anything in ax
  mov  al, [vidmode]              ; Set the predefined vga vidmode in the low byte of ax
  mov  bx, 0Fh                    ; Set bh (page nr) to 0, and bl (attribute) to white (F)
  int  10h                        ; switch mode
  ret

SetVBEMode:
  mov ah, 0x4f                    ; VBE function space
  mov al, 0x02                    ; VBE set mode function
  mov bx, 0x0118                  ; VBE mode number (118h = 1024x768)
  int 10h
  ret

InitTextMode:
  mov byte [vidmode], 03h
  call SetVidMode
  ret

InitGraphicsMode:
  push bp
  mov bp, sp        ; setup stack frame

  mov ax, [bp + 4]
  mov word [vidmode], ax
  call SetVidMode
  
  pop bp
  ret

WaitForVBlank:
  mov dx, 3DAh 
  in al, dx
  test al, 8
  jz WaitForVBlank
  ret

; https://wiki.osdev.org/VGA_Fonts#Get_the_copy_stored_in_the_VGA_BIOS
; in: es:di=4k buffer 8x16
; out: buffer filled with font
LoadVGABiosFont:
  push bp
	push ds
	push es
	
  ; ask BIOS to return VGA bitmap fonts
	mov ax, 1130h
	mov bh, 6
	int 10h
	; copy charmap
	push  es
	pop   ds
	pop   es
	mov   si, bp
	mov   cx, 256*16/4
	rep   movsd
	pop   ds
  pop   bp
  ret

%include 'resources/ibm_bios_font.s'

vgafont:          times 4096 db 0h
loadmsg:          db "Loading OS...", 0Ah, 0Dh, 00h
diskerror:        db "Disk error.", 00h
rebootpromptmsg:  db "Press any key to reboot.", 0Ah, 0Dh, 00h
rebootmsg:        db "Rebooting...", 0Ah, 0Dh, 00h
osnotfounderror:  db "No operating system found.", 0Ah, 0Dh, 00h
vgatestpromptmsg: db "Press any key to run VGA Test.", 0Ah, 0Dh, 00h

crtcreg:          dw 03D4h
mapmaskreg:       dw 03C4h

vgatmodebuf:      dw 0B800h
vgagmodebuf:      dw 0A000h
vidmode:          db 0h
bootsector:       dw 07C00h

bootdrive:        dw 0197h               ; address of bootdrive
endsignature:     db 041h, 042h         ; magic end of segment signature (ASCII AB)
times 0FFFFh - ( $ - $$ ) db 0FFh       ; Pad with nulls up to 64k bytes
