
LoadOS:
  lea  si, [osnotfounderror]      ; load address of no os found error into si
  call WriteString
  lea  si, [rebootpromptmsg]      ; Load address of reboot message into si
  call WriteString                ; print the string
  call KeypressWaitLoop
  call Reboot
  ret