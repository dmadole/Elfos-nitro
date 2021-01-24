; This software is copyright 2021 by David S. Madole.
; You have permission to use, modify, copy, and distribute
; this software so long as this copyright notice is retained.
; This software may not be used in commercial applications
; without express written permission from the author.
;
; The author grants a license to Michael H. Riley to use this
; code for any purpose he sees fit, including commercial use,
; and without any need to include the above notice.


           ; Include kernal API entry points

           include bios.inc
           include kernel.inc

           ; Define non-published API elements

version    equ     0400h
himem      equ     0442h
d_type     equ     0444h
d_msg      equ     0447h
d_readkey  equ     0454h
d_input    equ     0457h

           ; Pin and polarity definitions

#define    SERP    bn2
#define    SERN    b2
#define    SERSEQ  req
#define    SERREQ  seq

           ; Executable program header

           org     2000h - 6
           dw      start
           dw      end-start
           dw      start

start:     org     2000h
           br      checkver

           ; Build information

           db      1+80h              ; month
           db      14                 ; day
           dw      2021               ; year
           dw      0                  ; build
           db      'Written by David S. Madole',0


           ; Table giving addresses of jump vectors we need to update
           ; to point to us instead, along with offset from the start
           ; of the module in himem to repoint those to.

patchtbl:  dw      f_setbd, timalc - module
           dw      f_type, type - module
           dw      f_tty, type - module
           dw      d_type, type - module
           dw      f_read, read - module
           dw      d_readkey, read - module
           dw      f_input, input256 - module
           dw      d_input, input256 - module
           dw      f_inputl, input - module
           db      0


           ; Check minimum kernel version we need before doing anything else,
           ; in particular we need support for himem variable to allocate
           ; memory for persistent module to use.

minvers:   db      0,3,1              ; minimum kernel version needed

checkver:  ldi     minvers.1          ; pointer to version needed
           phi     r7
           ldi     minvers.0
           plo     r7

           ldi     version.1          ; pointer to running version
           phi     r8
           ldi     version.0
           plo     r8

           ldi     3                  ; check three bytes
           plo     rf

           sex     r8                 ; subtract from running version

versloop:  lda     r7                 ; compare minimum vs running versions
           sd
           irx
           lbnf    versfail           ; negative, running < minimum, so fail
           bnz     checkarg           ; positive, running > minimum, so pass

           dec     rf                 ; zero, so equal, keep checking
           glo     rf
           bnz     versloop           ; if we exit this versions are same


           ; Check supplied arguments to see if -k was specified, which
           ; requests installation into "spare" kernel memory at 1eff-1fff
           ; instead of high memory. This is a crutch to work around current
           ; application unawareness of high memory management and will be
           ; removed at some point in the future.

checkarg:  ldi     0                  ; by default, load to high mem
           plo     re

skipspac:  lda     ra                 ; look at next character, and if its
           bz      allocmem           ; a zero, we are done

           smi     ' '                ; skip any spaces
           bz      skipspac

           smi     '-'-' '            ; after any spaces needs to be a '-'
           lbnz    optfail

           lda     ra                 ; after the '-' needs to be a 'k'
           smi     'k'
           lbnz    optfail

           lda     ra                 ; the 'k' needs to be the last thing
           lbnz    optfail

           inc     re                 ; set flag to install to kernel mem

           glo     r2                 ; check that there is enough space
           smi     0f8h               ; after the kernel, the stack is the
           ghi     r2                 ; highest thing the kernel uses
           smbi    01dh
           lbdf    kernfail


           ; Check where we are installing in memory and setup the length
           ; and appropriate address of the block. If loading to high memory
           ; adjust the himem variable to memorialize the allocation.

allocmem:  ldi     (end-module).1     ; get length of code to install
           phi     rf
           ldi     (end-module).0
           plo     rf

           ldi     0                  ; target will always be page boundary
           plo     r8
           plo     r9

           glo     re                 ; check where we are installing
           bz     usehimem

           ldi     1eh                ; if kernel memory, this is easy, just
           phi     r8                 ; always setup the address for $1e00
           phi     r9
           br      copycode

usehimem:  ldi     himem.1            ; pointer to top of memory variable
           phi     r7
           ldi     himem.0
           plo     r7

           sex     r7                 ; subtract instructions reference highmem
           inc     r7                 ; move to lsb of address

           glo     rf                 ; subtract size to install from himem,
           sd                         ; discard result lsb since we want to
           ghi     rf                 ; round down to a page start anyway
           dec     r7
           sdb
           smi     1                  ; add extra page for $ff00 replacement
           phi     r8
           phi     r9                 ; r8 and r9 are where we will install

           dec     r8                 ; set himem to one less than install
           ghi     r8                 ; address to reserve memory block
           str     r7
           inc     r7
           glo     r8
           str     r7
           inc     r8                 ; restore to destination address


           ; Copy the code of the persistent module to the memory address
           ; that was just determined.

copycode:  ldi     module.1           ; get source address
           phi     r7
           ldi     module.0
           plo     r7

copyloop:  lda     r7                 ; copy code to destination address
           str     r8
           inc     r8
           dec     rf
           glo     rf
           bnz     copyloop
           ghi     rf
           bnz     copyloop


           ; Get the address of the next memory page above the code we
           ; just copied and copy the memory page at $ff00 that contains
           ; BIOS API jump vectors into it.

           ldi     0                  ; addresses at start of page
           plo     r7
           plo     r8
           plo     rf                 ; clear loop counter

           ldi     0ffh               ; point to BIOS
           phi     r7

           ghi     r8                 ; point to copy target
           adi     1
           phi     r8

copyff00:  lda     r7                 ; copy the page contents
           str     r8
           inc     r8
           dec     rf
           glo     rf
           bnz     copyff00

           ghi     r8                ; drop back to start of the page
           smi     1
           phi     r8


           ; Update kernel and BIOS hooks to point to our module code. At
           ; this point, R8 points to the new BIOS jump table in RAM, and
           ; R9 points to the base address of the module code in RAM.

           ldi     patchtbl.1        ; Get point to table of patch points
           phi     r7
           ldi     patchtbl.0
           plo     r7

           sex     r7                 ; add instructions will use table

ptchloop:  lda     r7                 ; a zero marks end of the table
           bz      callpage
           phi     ra
           adi     1
           bnz     isntffxx
           ghi     r8                 ; if the address is ffxx replace it
           phi     ra                 ; with equivalent in the copy in RAM
isntffxx:  lda     r7
           plo     ra

           inc     r7                 ; point to lsb of both addresses
           inc     ra                 ; ra needs to skip the lbr opcode
           inc     ra

           glo     r9                 ; add the offset in the table to the
           add                        ; base address in RAM and update the
           str     ra                 ; address at the patch point
           dec     r7
           dec     ra
           ghi     r9
           adc
           str     ra

           inc     r7                 ; point to next entry in table and
           inc     r7                 ; continue until all are done
           br      ptchloop


           ; The new call subroutine needs to know the page address of the
           ; new copy of the jump table in high memory so patch that in.

callpage:  glo     r9                  ; get address of ldi instruction
           adi     (ldipage-module).0
           plo     r7
           ghi     r9
           adci    (ldipage-module).1
           phi     r7

           inc     r7                  ; point to ldi argument and set
           ghi     r8
           str     r7

           ; Finally, modify R4 to point to the new call subroutine so that
           ; we intercept any SCRT subroutine calls going forward.

           glo     r9
           adi     (newcall-module).0
           plo     r4
           ghi     r9
           adci    (newcall-module).1
           phi     r4


           ; At this point we are ready to go but we need to first detect
           ; the baud rate. It might not be set yet if we are booting from
           ; direct entry at $ff00 but even if it is, the scheme we use is
           ; incompatible with BIOS due to the increased range.

           sep     scall               ; detect baud rate
           dw      timalc

           ldi     success.1
           stxd
           ldi     success.0
           stxd

output:    ldi     message.1
           phi     rf
           ldi     message.0
           plo     rf

           sep     scall
           dw      o_msg

           inc     r2
           lda     r2
           plo     rf
           ldn     r2
           phi     rf

           sep     scall
           dw      o_msg

           sep     sret

hookfail:  ldi     hookmsg.1
           stxd
           ldi     hookmsg.0
           stxd
           br      output

versfail:  ldi     vermsg.1
           stxd
           ldi     vermsg.0
           stxd
           br      output

optfail:   ldi     optmsg.1
           stxd
           ldi     optmsg.0
           stxd
           br      output

kernfail:  ldi     kernmsg.1
           stxd
           ldi     kernmsg.0
           stxd
           br      output

message:   db      'Nitro Soft UART Module Build 0.1 for Elf/OS',13,10,0
success:   db      'Copyright 2021 by David S Madole',13,10,0
optmsg:    db      'ERROR: Invalid option specified for command',13,10,0
vermsg:    db      'ERROR: Needs kernel version 0.3.1 or higher',13,10,0
hookmsg:   db      'ERROR: Read or write routines already hooked',13,10,0
kernmsg:   db      'ERROR: Insufficient kernel memory to install',13,10,0


           org     $ + 0ffh & 0ff00h

module:    ; Himem loadable module starts here

; ---------------------------------------------------------------------------
; Soft high-speed UART input and output routines
;
; This maintains the existing BIOS convention for the baud rate constant in
; register RE.1 although the actual timing values are different due to the
; higher resolution and baud rate supported.
;
; Only the high 7 bits of RE are used to hold the baud rate, and the lowest
; bit is used to indicate if local echo is enabled in the input routine.
;
; A baud rate of 0 is reserved for another meaning in BIOS so valid values
; are 1-127 after shifting right one bit to remove the echo flag.
;
; To increase the range of baud rates, this is normalized to a range of
; 0-252, with 1-64 representing 0-63 and 64-126 representing 67-252. This
; sacrifices resolution at lower baud rates where it doesn't matter since
; the percentage error becomes lower as the baud rate is lower.
;
; This supports baud rates of approximately 2000 to 21000 on a 4 Mhz clock,
; and proportionally higher or lower for other clocks. For a 1.8 Mhz PIXIE
; clock in particular, this supports approximately 900 to 9600 baud.


; ---------------------------------------------------------------------------
; Baud rate detection
;
; This measures the number of machine cycles time per serial bit. It does so
; by measuring across the 9 bits from the lead of the start bit to the lead
; of the stop bit. It can do so on any ASCII character input since it only
; relies on the highest data bit being zero, which is true for any character.
; Since the measurement loop is 9 machine cycles long, the measurement of
; 9 bits in loop units is equal to the measurement in cycles of one bit.

           org     $ + 0ffh & 0ff00h

timalc:    SERREQ                      ; Make output in correct state

timersrt:  ldi     0                   ; Wait to make sure the line is idle,
timeidle:  smi     1                   ;  so we don't try to measure in the
           nop                         ;  middle of a character, we need to
           SERN    timersrt            ;  get 256 consecutive loops without
           bnz     timeidle            ;  input asserted before this exits

timestrt:  SERP    timestrt            ; Stall here until start bit begins

           nop                         ; Burn a half a loop's time here so
           sex     r2                  ;  that result rounds up if closer

timecnt1:  phi     re                  ; Count up in units of 9 machine cycles
timecnt2:  adi     1                   ;  per each loop, remembering the last
           lbz     timedone            ;  time that input is asserted, the
           SERN    timecnt1            ;  very last of these will be just
           br      timecnt2            ;  before the start of the stop bit

timedone:  ldi     63                  ; Pre-load this value that we will 
           plo     re                  ;  need in the calculations later

           ghi     re                  ; Get timing loop value, subtract
           smi     23                  ;  offset of 23 counts, if less than
           bnf     timersrt            ;  this, then too low, go try again

           bz      timegood            ; Fold both 23 and 24 into zero, this
           smi     1                   ;  adj is needed for 9600 at 1.8 Mhz

timegood:  phi     re                  ; Got a good measurement, save it

           smi     63                  ; Subtract 63 from time, if less than
           bnf     timekeep            ;  this, then keep the result as-is

timedivd:  smi     3                   ; Otherwise, divide the excess part
           inc     re                  ;  by three, adding to the 63 we saved
           bdf     timedivd            ;  earlier so results are 64-126
        
           glo     re                  ; Get result of division plus 63
           phi     re                  ;  and save over raw measurement

timekeep:  ghi     re                  ; Get final result and shift left one
           shl                         ;  bit to make room for echo flag, then
           adi     2+1                 ;  add 1 to baud rate and set echo flag
           phi     re                  ;  then store formatted result and
           sep     sret                ;  return to caller

; ---------------------------------------------------------------------------
; Input

readspac:  smi     0                   ; Jumps here to set DF if a 1 bit

readmark:  glo     re                  ; Get current received character and
           shrc                        ;  shift right to put new bit at MSB,
           plo     re                  ;  move stack pointer to delay value

           ldn     r2                  ; Get timing constant and move SP back
           bdf     readloop            ;  to character, branch if not done

        ;; Done receiving bits

readdone:  smi     8                   ; Delay for half a bit to get to stop
           bdf     readdone            ;  bit, fractional part unimportant

           ghi     re                  ; Get baud rate constant and check
           shr                         ;  the echo flag, if set, we need to
           glo     re
           bdf     type                ;  echo, jump into type

           sep     sret                ;  and return to caller

        ;; Entry point is here

read:      ldi     0ffh                ; Preload character with all 1's, we
           plo     re                  ;  will look for a 0 to know when done

           ghi     re                  ; Uncompress the stored delay value:
           shr                         ;  shift to remove the echo flag,
           smi     1                   ;  then subtract 1 since 0 is reserved,
           str     r2                  ;  range is now 0-126, save on stack

           smi     63                  ; For values less than 63, leave as-is,
           bnf     readcomp            ;  for 63-126, change range to to 0-63

           shl                         ; Multiply by 2 to get range of 0-126,
           add                         ;  then add saved 63-126, giving range
           str     r2                  ;  of 63-252, update value on stack

readcomp:  ldn     r2                  ; Get half for delay to middle of start
           shr                         ;  bit where we will time other bits
           smi     4                   ;  from, do first delay loop subtract

readwait:  sex     r2
           SERP    readwait            ; Wait here until start bit comes in,
           bnf     readskip            ;  jump based on first delay subtract

           ; Receive loop resumes

readloop:  smi     4                   ; Delays for cycles equal to the next
           bdf     readloop            ;  higher multiple of 4 of value of D

readskip:  sdi     readjump.0-1        ; Calculate jump based on remainder of
           plo     r3                  ;  timing loop, which is -1 to -4

readjump:  skp                         ; Delay 5 cycles from here
           skp                         ;  4 cycles from here
           lskp                        ;  3 cycles from here
           ldi     0                   ;  2 cycles from here

           SERP    readspac            ; If not EF2 then bit is space, go set
           br      readmark            ;  DF, otherwise a mark, leave DF clear


; ---------------------------------------------------------------------------
; Output

type:      plo     re                  ; Save output character to stack
           stxd 

           ghi     re                  ; Uncompress the stored delay value:
           shr                         ;  shift to remove the echo flag,
           smi     1                   ;  then subtract 1 since 0 is reserved,
           str     r2                  ;  range is now 0-126, save on stack

           smi     63                  ; For values less than 63, leave as-is,
           bnf     typecomp            ;  for 63-126, change range to to 0-63

           shl                         ; Multiply by 2 to get range of 0-126,
           add                         ;  then add saved 63-126, giving range
           str     r2                  ;  of 63-252, update value on stack

typecomp:  ldn     r2                  ; Delay for one bit time before start
typedly1:  smi     4                   ;  bit so we can be called back-to-
           bdf     typedly1            ;  back without a start bit violation

           ldn     r2                  ; Get delay value, advance stack to
           SERSEQ                      ;  point to output character, then
typedly2:  smi     4                   ;  start sending start bit and delay
           bdf     typedly2            ;  for one bit in 4 cycle chunks

           sdi     typejmp2.0-1        ; Calculate jump for remainder left
           smi     0                   ;  from delay loop, set DF to shift a
           plo     r3                  ;  1 in to know when send is complete

           ; Main loop jumps back to here

typejmp2:  skp                         ; Delay 5 cycles from here
           skp                         ;  4 cycles from here
           lskp                        ;  3 cycles from here
           ldi     0                   ;  2 cycles from here

           glo     re                  ; Get character value to send,
           shrc                        ;  shift out lowest bit, update stack
           plo     re                  ;  to bit time delay value

           bz      typestop            ; If all bits clear, we are done, 
           bdf     type1bit            ;  otherwise jump if sending a 1 bit

           SERSEQ                      ; Send a zero bit
           ldn     r2                  ;  advance the stack pointer back
typedly3:  smi     4                   ;  to character value, then delay
           bdf     typedly3            ;  for one bit time

           sdi     typejmp2.0-1        ; Calculate the delay jump, this also
           plo     r3                  ;  returns to the start of the loop

type1bit:  SERREQ                      ; Send a one bit, this block of code
           ldn     r2                  ;  is the same as for the zero bit
typedly4:  smi     4                   ;  above but is factored out twice
           bdf     typedly4            ;  to meet timing constraints

           sdi     typejmp2.0-1        ; Calculate the delay jump, this also
           plo     r3                  ;  returns to the start of the loop

        ;; End of character

typestop:  SERREQ                      ; We are done sending, set the stop bit
           inc     r2                  ;  return input if this was an echo,
           ldn     r2

           sep     sret                ;  then return to the caller

           sep     r3                    ; jump to called routine
newcall:   plo     re                    ; Save D
           ghi     r6                    ; save last R[6] to stack
           sex     r2
           stxd
           glo     r6
           stxd
           ghi     r3                    ; copy R[3] to R[6]
           phi     r6
           glo     r3
           plo     r6
           lda     r6                    ; get subroutine address
           phi     r3                    ; and put into r3
           adi     1
           bnz     nochange
ldipage:   ldi     0ffh
           phi     r3
nochange:  lda     r6
           plo     r3
           glo     re                    ; recover D
           br      newcall-1             ; transfer control to subroutine

           org     $ + 0ffh & 0ff00h

input256:  ldi     0                   ; allow 256 input bytes
           phi     rc
           ldi     255
           plo     rc


           ; Input:
           ;   RC - maximum input size
           ;   RF - pointer to buffer
           ; Output:
           ;   RC - modified
           ;   RF - points to terminating zero
           ;   DF - set if control-c pressed

input:     ghi     re                   ; disable input echo but save first
           stxd                         ; so we can put back later
           ani     0feh
           phi     re

           glo     ra                   ; free up as a working register
           stxd
           ghi     ra
           stxd

           ldi     0                    ; pre-populate terminating 0
           str     rf
           plo     ra                   ; zero cursor position counter

inptloop:  sep     scall                ; get next character
           dw      o_readkey
           plo     re                   ; save a copy

           smi     32                   ; is it a printable char?
           bdf     printchr

           smi     3-32                 ; is it control-c?
           bz      inptdone 

           smi     4-3                  ; is it control-d?
           bz      deletech 

           smi     6-4                  ; is it control-f?
           bz      insertch 

           smi     8-6                  ; is it control-h?
           bz      backspac

           smi     12-8                 ; is it control-l?
           bz      forespac

           smi     13-12                ; is it return?
           bz      inptdone

           smi     21-13                ; is it control-u?
           bz      wipeline

           br      inptloop             ; ignore anything else


           ; Add a character to input, appending to end and extending if we
           ; are at the end of existing input, or overwriting if we are in
           ; the middle somewhere. Either way output the character too.

printchr:  ldn     rf                   ; are we at the end of current input?
           stxd                         ; remember answer
           bnz     inptchar

           glo     rc                   ; if extending, check if theres room
           bz      inptloop

inptchar:  glo     re
           str     rf
           inc     rf

           sep     scall                ; and output
           dw      o_type

           inc     ra                   ; move cursor to right

           inc     r2
           ldn     r2                   ; recheck if we are extending
           bnz     inptloop

           str     rf                   ; if so, add new zero terminator
           dec     rc                   ; and decrease the input limit
           br      inptloop


           ; Control-H moves the cursor left without changing the buffer.

backspac:  glo     ra                   ; ignore if we are all the way left
           bz      inptloop

dobacksp:  glo     re                   ; output the backspace
           sep     scall
           dw      o_type

           dec     ra                   ; move the cursor and buffer pointer
           dec     rf
           br      inptloop


           ; Control-L moves the cursor right without changing the buffer.

forespac:  ldn     rf                   ; if at the end then do nothing
           bz      inptloop

           sep     scall                ; output char at cursor to move right
           dw      o_type

           inc     ra
           inc     rf
           br      inptloop


           ; Either a control-c or return has been typed so we are done.
           ; The only difference between the two is how DF is set at exit.

inptdone:  lda    rf                    ; make rf point to terminator at end
           bnz    inptdone
           dec    rf

           ldi    13                   ; output carriage return line feed
           sep    scall
           dw     o_type
           ldi    10
           sep    scall
           dw     o_type

           glo    re                   ; set df=1 if it is a control-c
           sdi    3
           sdi    0

           inc    r2                    ; restore saved register
           lda    r2
           phi    ra
           lda    r2
           plo    ra

           ldn    r2                   ; restore echo flag and return
           phi    re
inptsret:  sep    sret


           ; Wipe out the entire input

wipeline:  glo    ra
           bz     wipespac

           ldi    8
           sep    scall
           dw     o_type

           dec    ra
           dec    rf
           br     wipeline

wipespac:  ldn    rf
           bz     wipeback

           ldi    32
           sep    scall
           dw     o_type

           inc    ra
           inc    rc
           inc    rf
           br     wipespac

wipeback:  glo    ra
           bz     wipedone

           ldi    8
           sep    scall
           dw     o_type

           dec    ra
           dec    rf
           br     wipeback

wipedone:  str    rf
           br     inptloop


           ; Insert space character into input at current location

insertch:  glo     ra
           stxd
           ldi     0
           plo     ra

           ldi     32
           plo     re

insloop1:  ldn     rf                   ; push characters out until end
           stxd
           glo     re
           str     rf
           bz      insloop2

           sep     scall                ; output character as we go
           dw      o_type

           inc     r2
           ldn     r2
           plo     re
           inc     rf
           inc     ra
           br      insloop1

insloop2:  glo     ra                  ; back to starting point outputting
           bz      instdone

           ldi     8
           sep     scall
           dw      o_type

           dec     ra
           dec     rf
           br      insloop2

instdone:  inc     r2
           inc     r2
           ldn     r2
           plo     ra
           br      inptloop

           ; Deletecharacter from input at current location

deletech:  ldn     rf                   ; if at end then nothing to do
           bz      inptloop

           glo     ra
           stxd
           ldi     0
           plo     ra

delloop1:  ldn     rf                   ; push characters out until end
           bz      delspace

           inc     rf
           ldn     rf
           dec     rf
           str     rf

           sep     scall                ; output character as we go
           dw      o_type

           inc     rf
           inc     ra
           br      delloop1

delspace:  ldi     32
           sep     scall
           dw      o_type

delloop2:  glo     ra                  ; back to starting point outputting
           bz      deledone

           ldi     8
           sep     scall
           dw      o_type

           dec     ra
           dec     rf
           br      delloop2

deledone:  inc     r2
           ldn     r2
           plo     ra
           br      inptloop

end:       ; That's all folks!


