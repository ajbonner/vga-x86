[BITS 16]

gdtr dw 0 dd 0

reloadSegments:
  ; Reload cs register containing code selector:
  jmp 08h:reload_cs     ; 0x08 points at the new code selector

  .reload_cs:
    ; Reload data segment registers:
    mov   ax, 010h      ; 10h points at the new data selector
    mov   ds, ax
    mov   es, ax
    mov   fs, ax
    mov   gs, ax
    mov   ss, ax
    ret