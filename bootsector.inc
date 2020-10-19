bootsector:
  iOEM:         db  "AaronsOS"    ; OEM String
  iSectSize     dw  0200h         ; bytes per sector (200h/512d)
  iClustSize    db  1             ; num sectors per cluster
  iResSect      dw  1             ; num reserved sectors
  iFatCnt       db  2             ; fat copy count
  iRootSize     dw  224           ; size of root directory
  iTotalSect    dw  2880          ; total num of sectors if over 32 MB
  iMedia        db  0F0h          ; media decriptor
  iFatSize      dw  9             ; size of each FAT
  iTrackSect    dw  9             ; sectors per track
  iHeadCnt      dw  2             ; number of read-write heads
  iHiddenSect   dd  0             ; number of hidden sectors (e.g. bad sectors)
  iSect32       dd  0             ; number of secutors for over 32 MB
  iBootDrive    db  0             ; holds drive that boot sector came from
  iReserved     db  0             ; reserved, empty
  iBootSign     db  029h          ; extended boot sector signature
  iVolID        db  "DISK"        ; disk serial
  acVolumeLabel db  "BOOT       " ; volume label
  acFSType      db  "FAT16   "    ; file system type