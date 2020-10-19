[BITS 16]
[ORG 7C00h]

start:
  jmp near main

;%include "bootsector.inc"

main:
  ; Setup segments:
  cli
  mov  [bootdrive], dl            ; save what drive we booted from (should be 0x0)
  mov  ax, cs                     ; CS = 0x0, since that's where boot sector is (0x07c00)
  mov  ds, ax                     ; DS = CS = 0x0
  mov  es, ax                     ; ES = CS = 0x0
  mov  ss, ax                     ; SS = CS = 0x0
  mov  di, ax
  mov  sp, 07C00h                 ; Stack grows down from offset 0x7C00 toward 0x0000.
  mov  bp, sp
  sti

  ; Setup initial vidmode - effective way to clear screen
  call SetVidMode

  ; Load stage2 from boot disk
  call LoadStage2

  ; long jump to 1000h:0200h (0x10200h physical) where our stage 2 lives
  jmp 1000h:0200h

LoadStage2:
  push bp
  mov bp, sp

  ; arg 1 bootdrive
  mov bl, byte [bootdrive]
  xor bh, bh
  push bx
  ; arg 2 es (dest buffer segment)
  push 01000h
  ; arg 3 bx (dest buffer index full address es:bx)
  push 0h
  ; arg 4 number of bytes to read
  push 0FFFFh ; 64k
  ; arg 5 drive start of disk byte offset
  push 0h     

  ; init bootdisk read procedure
  mov dl, [bootdrive]             ; select drive to read from
  xor ax, ax
  xor bx, bx
  xor cx, cx
  call ReadFromFloppy
  
  mov sp, bp
  pop bp
  ret

; expects es:di to be initialised to a buffer to be read into
ReadFromFixedDisk:
  mov dl, byte [bootdrive]
  call ResetDisk
  ret

; @see  https://stanislavs.org/helppc/int_13-2.html
; @args bootDriveIndex, destSegment, destIndex, numBytesToRead, byteOffset
ReadFromFloppy:
  push bp
  mov bp, sp

  mov dl, byte [bp+12]  ; arg 1 drive index to read from
  mov bx, [bp+10]       ; arg 2 dest buffer segment
  mov es, bx            ; "
  mov bx, [bp+8]        ; arg 3 dest buffer index
  ; bp+6 numBytesToRead
  ; bp+4 startOffset

  call ResetDisk

  ; floppy 18 sectors per track, 80 tracks per side and 2 sides (heads), first 18 sectors on h0,t0, second 18 sectors h1,t0 and so on...
  mov dh, 0                       ; start at drive head 0
  mov ch, 0                       ; start at track (cyl) 0
  mov cl, 1                       ; start at sector 1 (1 indexed, first sector, i.e. including the bootsector)

  .read:
    mov al, 18                      ; we want to read 18 sectors (512 byte sectors / our stage2 block is 64k) 9216 bytes
    mov ah, 02h                     ; select read sectors bios service
    int 13h                         ; call bios drive service
    jc ReadFail                     ; terminate with error when carry flag is set

    add bx, 18*512                  ; inc our target memory address by 18 sectors * 512 bytes
    xor dh, 1                       ; do bit flip to flip between side a/side b, track 0 sectors 1-18 side a, track 0 19-36 side b
    cmp dh, 1                       
    je .read                        ; if we're now on head/side 1, we need to read again
    inc ch                          ; inc track number
    cmp ch, 3                       ; we want to read 6 tracks (3 tracks over 2 sides) contaning 18 sectors each (18 per side double sided disk), of which each sector = 512 bytes
    jne .read

    ; read last 20 sectors to read in our 64k total
    mov al, 18
    mov ah, 02h
    int 13h 
    jc ReadFail
    add bx, 18*512

    xor dh, 1
    mov al, 2
    mov ah, 02h
    int 13h 
    jc ReadFail

  .endread:
    mov sp, bp
    pop bp
    ret

; 1 arg, dl, which contains bios drive index
ResetDisk:
  push ax
  xor ax, ax                      ; subfunction 0
  int 13h                         ; call interrupt 13h
  jc ResetDisk                    ; retry as error if carry set
  pop ax
  ret

ReadFail:
  lea si, [bootfailmsg]
  call WriteString
  call HaltCatchFire

SetVidMode:
  xor  ax, ax                     ; Rub out anything in ax
  mov  al, 03h                    ; Set the predefined vga vidmode (textmode 03) in the low byte of ax
  mov  bx, 0Fh                    ; Set bh (page nr) to 0, and bl (attribute) to white (F)
  int  10h                        ; switch mode
  ret

WriteString: 
  lodsb                   ; load byte at ds:si into al (advancing si)
  or     al, al           ; test if character is 0/null (null-terminated string ending)
  jz     .done            ; jump to end if last char was null
 
  mov    ah, 0Eh          ; Subfunction 0xe of int 10h (video teletype output)
  mov    bx, 9            ; Set bh (page nr) to 0, and bl (attribute) to white (9)
  int    10h              ; call BIOS interrupt.
 
  jmp    WriteString      ; Repeat for next character.
 
  .done:
    ret

HaltCatchFire:
  cli
  hlt
  jmp HaltCatchFire
  
bootmsg:                db "Loading stage 2...", 0x0A, 0x0D, 0x00
bootfailmsg:            db "Error reading stage2 from device...", 0x0A, 0x0D, 0x00
times 509 - ( $ - $$ )  db 0h           ; Pad with nulls up to 510 bytes (excl. boot magic)
bootdrive:              db 0h
bootmagic:              dw 0AA55h       ; magic word for BIOS 