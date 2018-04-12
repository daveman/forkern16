        PAGE ,132
	TITLE FORKERN: DAVE'S FORTH KERNEL
;* Remember - word Pointer = SI (relative to DS = CS)
; word Register = BX (points to current word to execute)
; word Stack = SP (relative to SS usual - the system stack)
; Data Stack = SP (relative to SS copied from DI when needed)
; Dictionary Pointer = DP (a word offset into the CS)

ESEG    SEGMENT PARA PUBLIC 'A'
ESEG    ENDS

;DSEG SEGMENT PARA PUBLIC 'DATA'
;DSEG ENDS
;******************************************************************************
;your basic dos addressability stuff...
CSEG    SEGMENT PARA PUBLIC 'CODE'
ASSUME  CS:CSEG,SS:STACK                ;ALREADY SET BY DOS LOADER

ENTPT   PROC    FAR                     ;ENTRY POINT FROM DOS
        MOV     AX,cs                   ;SET UP ADDRESSABILITY T0
        MOV     DS,AX                   ; THE DATA SEGMENT
        ASSUME  DS:cseg                 ;TELL ASSEMBLER WHAT I JUST DID
        mov     es,ax
        ASSUME  ES:eseg
        jmp     init                    ;skip embedded data portion

;******************************************************************************
cr      equ     13                      ;carraige return
lf      equ     10                      ;line feed
bs      equ     8                       ;back space
tab     equ     9
topstk  equ     2048                    ;2046 byte data stack

;program data - defined here because of coexistent code & data segs

dp      dw      0               ;must be initialized by the outer interpreter
                                ; to point to the top of the dictionary

odp     dw      0               ;temp for saving dp during definitions
fvoc    dw      llink           ;forth's vocabulary area
current dw      fvoc
context dw      fvoc
saves   dw      0               ;stores the contents of the extra seg reg
savds   dw      0               ;...
svstk   dw      topstk          ;stack pointer storage during outer intrp.
radix   dw      10
inptr   dw      1               ;ptr to current char in input buffer
buflen  dw      0
rhandle dw      0
attribute dw    1fh             ;attribute variable used for char output
syscrsr dw      0               ;system cursor row byte, column byte
delim   db      ' '             ;current token delimiter
leadin  db      ' '             ;leading character skipped by token routine
cvt     db      32              ;upper to lower case -if desired.
inbuf   db      256 dup (?)     ;256 byte input buffer
numset  db      '0123456789ABCDEFGHIJKLMNOPQRSTUVUXYZ'
dosin   db      1               ;dos input code (defaults to input with echo
mode    db      0               ;default to execute mode
filin   db      0               ;input from keyboard or file? (default file)
uns     db      0               ;is output unsigned?
ok      db      ' ok.',cr,lf,0
intro   db      ' welcome to Graftil',cr,lf,' Version 1.0',cr,lf,0
nferr   db      '?',cr,lf,0
msg1    db      cr,lf,'Primary ONE, reporting for duty!',cr,lf,0
runmes  db      cr,lf,'Attempting to run it Dave ...',cr,lf,0
tkmsg   db      cr,lf,'Stack reset due to overflow/underflow',cr,lf,0
lderrmes db     ' Open load error ',cr,lf,0
readmes db      ' Load error ',cr,lf,0

even
scratch db      255 dup (?)
escratch db     0
debug   equ     1               ;1 means enable debug
        IF debug
bugflg  dw      0
bottom  dw      0               ;storage for initial ss to calc nest level from
deepw   dw      3
        ENDIF

;*******************************************************************************
;support macros...
fjump   macro                   ;macro to jump to the address stored
        lodsw                   ; in the word following this forth word
        mov     si,ax
        lodsw                   ;next & run
        mov     bx,ax
        IF debug
        call    ckbug
        ENDIF
        jmp     [bx]
        endm

skip    macro                   ;macro to continue execution skipping 1 word
        inc     si
        inc     si
        jmp     next
        endm

nxtrun  macro                   ;code for an inline next & run sequence
        lodsw
        mov     bx,ax
        IF debug
        call    ckbug
        ENDIF
        jmp     [bx]
        endm

inc2    macro   reg             ;increment twice
        inc     reg
        inc     reg
        endm

dec2    macro   reg             ;decrement twice
        dec     reg
        dec     reg
        endm

tobxdp  macro   reg             ;get the dp, stuff into it and advance it...
        mov     bx,dp
        mov     word ptr [bx],reg
        inc     bx
        inc     bx
        endm

todp    macro   reg
        mov     word ptr [bx],reg
        inc     bx
        inc     bx
        endm

bxtodp  macro
        mov dp,bx
        endm

;******************************************************************************
;The inner interpreter...

inner proc near
        IF debug
semin   db      4,'semi '
semill  dw      0
        ENDIF
semi    dw      semic
semic:  pop     si              ;denest 1 level
next:   lodsw                   ;get the start address of this word
                                ;& advance to the next one { mov ax,[si++] }
mov     bx,ax                   ;get the start address from ax

        IF debug                ;assemble with debug?
        call    ckbug
        ENDIF

run:    jmp     [bx]            ;jump through to the next routine
                                ;next=14 cycles !! (down from 17)
colon:  push    si              ;save from whence we came
        mov     si,ax           ;indirect to the next entry
        inc2    si              ;advance to 1st word of secondary
        lodsw                   ;next & run (run & next ??)
        mov     bx,ax           ;mov ax,[si++]
        IF debug                ;assemble with debug?
        call    ckbug
        END IF

        jmp     [bx]            ;20 cycles !! (up from 19)

inner endp
;******************************************************************************
;inner interpreter debug support module
        IF debug
savbuf  dw      80 dup (7)      ;buffer for 25th line
Hrtctr  dw      0
savca   dw      0
ckbug proc near
        cmp     deepu,0
        jne     pushem
        ret
pushem: push    ax
        push    bx
        push    cx
        push    dx
        push    radix
        mov     radix,16
        mov     savca,bx
        xor     dx,dx           ;port #1
        mov     ah,3            ;serial port status cmd
        int     14h
        and     ah,1            ;data ready bit set ?
        je      nokey           ;nothing
getser: xor     dx,dx
        mov     ah,2            ;get the char
        int     14h
        cmp     al,','          ;- toggles debug on and off
        jne     ckplus
        xor     bugflg,1
ckplus: cmp     al,' '          ;if + then pause
        jne     nokey
waitlp: xor     dx,dx
        mov     ah,3
        int     14h
        and     ah,1
        je      waitlp
        jne     getser
nokey:  cmp     bugflg,1
        je      nebug
        jmp     ebug

nebug:
        mov     ax,bottom       ;calc return stack depth (nesting level)
        mov     cx,sp
        sub     ax,cx
        shr     ax,1
        sub     ax,6
        emp     ax,deepw        ;ck nesting level
        jg      ebug
        push    ax
        mov     al,'['
        call    sertty
        pop     ax
        inc     bugflg
        call    outnum
        mov     cx,topstk       ;get stack depth
        sub     cx,di           ;calc depth
        shr     cx,1
        mov     ax,cx
        push    cx
        inc     bugflg          ;tell outnum its debug
        call    outnum          ;outnum includes hook to check
        mov     al,']'          ;status of bugflg when in
        call    sertty          ;debug mode...
        mov     al,' '
        call    sertty
        pop     cx
        cmp     cx,8
        jl      wrtstk
        mov     cx,8
wrtstk: cmp     cx,0
        jte     wait
        mov     urtctr,cx
        dec     cx
        mov     bx,di
        add     bx,cx
        add     bx,cx
wrtslp: push    bx
        mov     ax,ss:[bx]
        inc     bugflg
        call    gutnum
        pop     x
        dec2    bx
        dec     wrtctr
        jne     wrtslp
wait:   mov     bx,savca        ;get ptr to next ca to be executed
        sub     bx,7            ;back up to ascii name
        mov     cx,5            ;all are 5 chars...
wrtnam: mov     al,[bx]
        call    sertty
        inc     bx
        loop    wrtnam
        mov     al,cr
        call    sertty
        mov     al,lf
        call    sertty
        ;mov ah,0               ;wait for keypress...
        ;int 22
ebug:   pop     radix
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        ret
sertty: push    dx
        xor     dx,dx
        mov     ah,1
        int     14h
        pop     dx
        ret
ckbug endp
        ENDIF

;******************************************************************************
;The outer interpreter...

outer proc near                 ;***************************************
init:   cld                     ;clear the direction flag for inc
        mov     savds,ds        ;keep these for quick reference
        mov     saves,es
        mov     ax,offset edic  ;copy the dictionary pointer in
        mov     dp,ax
        mov     odp,ax
        mov     ax,25
        call    scrollit
        mov     si,offset intro
        call    messout
        IF debug
        ;debug initialization
        xor     dx,dx           ;serial port 1
        mov     ax,dx           ;ah=0 initialize port
        mov     al,11100111b    ;4800 baud, 8 bits no parity 2 stop
        int     14h             ;initialize port
        mov     bottom,sp
        ENDIF
                                ;************************************
                                ;Read & deal with tokens in the input buffer
getnxt: call    token
        call    search          ;returns cc = if found
        jne     nofind
        mov     dl,[bx-6]       ;is this an immediate word
        test    dl,80h
        jne     jrunit          ;if so, run (outside branch range...)
        test    mode,0ffh       ;compile mode ?
        jne     addit
jrunit: jmp     runit
addit:  inc2    bx              ;bx pts to link address, code address is next
        mov     di,dp
        mov     [di],bx         ;add it to the dictionary
        inc2    di
        mov     dp,di
        jmp     getnxt
nofind: call    cknum
        jne     nofnder
        test    mode,Offh
        je      stackit
        mov     di,dp
        cmp     dblflg,-1
        jne     dbllit
        mov     bx,offset litterca ;add a numeric litteral to the
        mov     [di],bx         ;word being defined
        inc2    di
        mov     [di],ax         ;has the converted number frm cknum
        inc2    di
        mov     dp,di
        jmp     getnxt

dbllit: mov     bx,offset dlittca
        mov     [di],bx
        inc2    di
        mov     dx,hisave
        mov     [di],dx
        inc2    di
        mov     [di],ax
        inc2    di
        mov     dp,di
        jmp     getnxt

stackit: mov    di,sp           ;save the system stack
        mov     sp,svstk        ;load in the data stack
        push    ax
        cmp     dblflg,-1
        je      estackit
        push    hisave

estackit: mov   svstk,sp
        mov     sp,di           ;go back to system stack
        jmp     getnxt
nofnder: mov    si,dp
        inc si
        call    messout
        mov     si,offset nferr
        call    messout
        test    mode,1          ;Here we compiling ?
        jne     skpnxt
        jmp     getnxt
skpnxt: xor     ax,ax           ;reset mode _
        mov     mode,al
        mov     buflen,ax       ;reset the input buffer
        mov     filin,0         ;go back to manual input
        inc     ax
        mov     inptr,ax        ;signal new rdfrmbuf
        mov     ax,odp          ;reset dictionary to "pre-definition"
        mov     dp,ax
        jmp     getnxt

                                ;**********************************
                                ;code to take me in and out of the
                                ; inner interpreter....
runit:
        IF DEBUG
        mov     sp,bottom       ;save current stack as debug reference
        ENDIF
        mov     di,svstk        ;restore the data stack pointer
        lea     bx,[bx+2]       ;put code address in exec word list
        mov     exec,bx
        mov     si,offset exec  ;pt si to exec uord
        jmp     next            ;go for it

endrun: mov     svstk,di        ;save the stack pointer
        jmp     getnxt          ;see what else to do...

                                ;***************************************
token proc near                 ;make a temporary header from next token
        call    getchar         ;get next c ar from buffer to al
        cmp     al,tab          ;tabs are spaces...
        jne     cpl
        mov     al,' '
cpl:    cmp     al,leadin       ;scan past white space
        je      tokzn
        mov     di,dp
        mov     bx:0            ;di will count length (1 skips len byte)
cpt:    cmp     al,delim        ;end of the token ?
        je      etok 
        inc     bx              ;count length
        mov     [bx+di],al      ;copy char to top of dictionary
        call    getchar         ;get next
        cmp     al,tab          ;tabs are spaces
        jne     cpt
        mov     al,' ' 
        jmp     cpt             ;ck it... _
etok:   mov     [di],bl         ;store the length in the header
        inc     bx
        mov     word ptr [bx+di],' ' ;store delimiter for cknum
        ret                     ; with a trailing 0
        token endp

                                ;******************************************
                                ;Search for the token on top of the dict.
search  proc near               ;sets cc to eq if found, & bx pts to link
        mov     bx,dp           ;get the top of the dictionary
        lea     si,[bx+1]       ;get start address in case we match on len
        xor     ax ax           ;zero ax
        mov     al:[bx]         ;get the length from the new token
        mov     bx,context      ;get ptr to top link
        srch1:  mov bx,[bx]     ;advance 1 link
        cmp     bx,0            ;have we exhausted the list?
        je      srch3
        mov     dl,[bx-6]       ;compare to the length 8 the crnt link
        and     dl 7fh          ; (with com ile bit reset)
        cmp     al,dl
        jne srch1               ;if no-go skip the actual compare
        
                                ;compare the two tokens
        mov     cx,ax
        cmp     al,6            ;is this longer than 5?
        jl      srch2           ;if not then lets go with_it
        mov     cx,5            ;if so, we need to limithit to S
srch2:  push    si              ;save si in-case no matc
        lea     di,[bx-5]       ;get the start of the current word
        repe    cmpsb           ;compare the strings
        pop     si              ;get back si before exit
        jne     srch1           ;<> then try again
        jmp     srch6           ;otherwise quit with = status
srch3:  cmp     bx,1            ;gauranteed <> on cc on exit
srch6:  ret
search endp

                                ;*****************************************
getchar proc    near            ;get a character and filter returns...
        push    bx
        mov     bx,inptr
        cmp     bx,buflen       ;buffer empty 7
        jge     skpr
        jmp     rdfrmbuf

                                ;read 1 line from current I/O device
skpr:   cmp     filin,0         ;are we reading from keyboard
        jne     fread
        push    si              ;echo ok message...
        mov     si,offset ok
        call    messout
        pop     si
        mov     buflen,0        ;reset buffer
nxkey:  mov     ah,0            ;service 1 - wait for key
        int     16h             ;bios keyboard int
        cmp     al,bs           ;if backspace, fix buffer
        jne     nobak
        
        mov     al,bs           ;back space char
        call    clrtty
        mov     al,' '          ;destroy char under cursor
        call    clrtty
        mov     al,bs           ;put cursor back
        call    clrtty
        cmp     buflen,0
        je      nxkey
        dec     buflen          ;back up if not at margin
        jmp     nxkey

nobak:  mov     bx,buflen       ;copy char to buffer
        mov     inbuf[bx],al
        inc     buflen
        cmp     al,cr           ;if cr don't print it...
        je      esci
        call    clrtty
        jmp     nxkey

fread:                          ;read a buffer-ful from the current device
        push    cx
        push    dx
        mov     bx,rhandle      ;get the handle that open left
        mov     cx,255          ;get, at most 255 chars
        mov     dx,offset inbuf
        mov     ah,3fh          ;read function
        mov     in tr,0         ;new buffer - reset ptr
        int     21E
        pop     dx
        pop     cx 
        jae     goodrd          ;if carry 1S set we had trouble
        mov     filin,0         ;go back to keyboard input
        push    dx
        mov     si,offset readmes
        call    messout
        mov     ax,0
        pop     dx

goodrd: mov     buflen,ax
        cmp     ax,0            ;eof at 1st attempt?
        jne     esci
        mov     filin,0         ;go back to keyboard input
        mov     inbuf,' '       ;return a dummy space to keep things going
        mov     bx,rhandle      ;& close the file
        mov     ah,3eh
        int     21h
esci:   xor     bx,bx           ;start at first char
        mov     inptr,bx        ;reset the mem char ptr.

rdfrmbuf: mov   al,inbuf[bx]    ;get char to al for uniformity...
        inc     inptr           ;advance buf ptr
        pop     bx
        cmp     al,cr           ;if it's a carraige return
        jne     ck2
        mov     at,' '          ;replace with space
        ret
ck2:    cmp     al,lf           ;or a line feed
        jne     eck
        mov     al,' '
        ret
eck:    cmp     al,97           ;is it <a
        jl      ck26
        cmp     al,122          ;or >2
        jg      ck26
        sub     al,cvt          ;make upper
        ret
ck26:   cmp     al,26           ;EOF char?
        jne     eac             ;if so, don't wait for eof on char count
        mov     buflen,0        ; go ahead and switch back
        mov     inptr,1
        mov     filin,0
        mov     al,' '
eac:    ret
getchar endp
outer endp

clrtty  proc    near
        push    bx
        push    cx
        push    dx
        push    ax q            ;for erso bios bug.“
        mov     dx,syscrsr      ;get fork cursor pos.
nowrap: cmp     al,tab
        je      rtab
nourap1: cmp    al,bs
        je      bscode
        cmp     al,cr
        je      crcode
        cmp     al,lf
        je      lfcode
        mov     cx,1
        mov     bx,attribute
        mov     ah,9
        int     10h
        inc     dx
extty:  cmp     dl,80
        jge     nlcode
extty2: cmp     dh,25           ;off the edge?
        jge     calscrl
storex: mov     syscrsr,dx
        xor     bx,bx
        mov     ah,2            ;set cursor function
        int     10h             ;dx has pos., bx has page
        pop     ax              ;for erso (which trashes ax)
        pop     dx
        pop     cx
        pop     bx
        ret
bscode: dec     dx
        cmp     dl,0
        jg      exbs
        xor     dl,dl
exbs:   mov     ah,2            ;back up cursor then exit
        int     10h
        jmp     extty

crcode: xor     dl,dl
        jmp     storex

lfcode: inc     dh
        jmp     extty2

nlcode: add     dx,0b0h         ;uraps 1 digit to bh & resets bl
        jmp     extty2

calscrl: mov    ax,1            ;scroll 1 line
        call    scrollit
        mov     dx,24*256       ;start at beginning of line...
        jmp     storex

rtab:   xor     ax,ax
        mov     cx,8
        mov     al,dl           ;take column & /8
        div     cl
        sub     cl,ah           ;subtract remainder from 8 to get nearest stop
tablp:  mov     ax,' '
        call    clrtty
        loop    tablp
        pop     ax              ;for erso
        pop     dx
        pop     cx
        pop     bx
        ret

scrollit: mov   bh,byte ptr attribute ;loH byte of attribute
        push    bp
        mov     ah,6            ;assume up
        cmp     al,0
        jge     scup
        inc     ah
        neg     al
scup:   mov     cx,0            ;upper left of region is 0,0
        mov     dx,79+(24*256)
        int     10h
        pop     bp
        ret
clrtty endp

messout proc near               ;write out asciiz string
        mov     al,[si]
        inc     si
        cmp     al,0
        je      emessout
        call    clrtty
        jmp     messout
emessout: ret
messout endp

cknum proc near                 ;is it number, & what is its value
        mov     dblflg,-1       ;assume 16 bit
                                ;alternate entry point used by convert..
cvnum:  mov     di,dp           ;get start of word
        inc     di              ;skip length byte
        mov     cx,radix        ;get the current number base
        mov     bp,cx
        dec     bp
        xor     ax,ax
        mov     bx,ax
        mov     sflg,ax         ;assume positive
        mov     dx,ax           ;dx is high word of possible 32 bitter
        mov     hisave,ax
        mov     bl,[di]         ;get 1st char
        cmp     bl,'-'
        jne     pos
        inc     sflg            ;negate sign
        inc     di              ;move to 1st digit
pos:    mov     bl,[di]
        cmp     bl,' '          ;finished?
        je      econ
        sub     bl,'0'
        jl      econ2           ;if a<0 error in conversion or 32 bit indicator
        cmp     bl,9            ;if digit go on
        jle     ecvt
        sub     bl,7            ;adjust alphas
        cmp     bl,10           ;was it between 9 & A
        jl      econ1
ecvt:   cmp     bx,bp           ;radix -1
        jg      econ            ;it may be a colon (32 bit indicator)...see...
mult:   mul     cx              ;multiply current total by base
        cmp     hisave,0        ;is there anything in the top 16 bits
        jne     cntdbl          ;if not, skip high word handling
        cmp     dx,0
        je      exdbl
cntdbl: push    ax              ;push low answer
        push    dx              ;push high answer
        mov     ax,hisave       ;get current high 16
        mul     ex              ;and multiply it
        pop     dx              ;get high num from low 16 multiply
        add     dx,ax           ;and add to low 16 multiply
        pop     ax 
exdbl:  add     ax,bx           ;add the newest digit in
        adc     dx,0            ;add the carry to the high word
        mov     hisave,dx
        inc     di
        jmp     pos 

econ:   xor     dx,dx
        cmp     dx,sflg         ;negate?
        je      econ1
        cmp     dblflg,-1
        jne     neg2
        neg     ax
        cmp     dl,dl           ;set cc =
econ1:  ret
neg2:   xor     ax,0ffffh       ;reverse the bits in ax
        mov     dx,hisave
        xor     dx,0ffffh       ;reverse the high bits
        add     ax,1            ;2's complement
        adc     dx,0            ;that's it
        mov     hisave,dx
        cmp     dl,dl           ;return no error
        ret
econ2:  cmp     bl,','-'0'      ;ck for ',./-'
        jl      econ1           ;if lower quit & return error (not num)
        mov     dblflg,0        ;if higher it must be, so assume double
        cmp     bl,'.'-'0'      ;if period, count dec. places
        jne     nper
        push    bx
        push    cx
        mov     bx,dp           ;decimals=dp+len-di
        xor     cx,cx
        mov     cl,[bx]         ;len is byte @ dp
        add     bx,cx
        sub     bx,di           ;- length
        mov     dblflg,bx       ;and save...
        pop     cx
        pop     bx
nper:   inc     d1
        jmp     pos
econ3:  cmp     bl,':'-'0'      ;ck for ':
        jne     econ1
        inc     di
        jmp     pos 
sflg    dw      0               ;sign flag
dblflg  dw      0               ;double length flag
hisave  dw      0               ;high word of double
cknum endp

                                ;***********************************
outnum proc near                ;routine to output a number in crnt base
        push    di
        mov     bx,radix        ;number is passed in ax
        xor     cx,cx           ;assume unsigned
        mov     dx,' '
        push    dx              ;push trailing space
        cmp     uns,0
        jne     out1            ;unsigned if not decimal
        cmp     ax,0            ;is it neg.
        jge     out1
        mov     cx,'-'
        neg     ax
out1:   xor     dx,dx           ;used as high byte of dividend
        idiv    bx
        mov     di,dx           ;get index to char
        mov     dl,numset[di]   ;read it
        push    dx
        cmp     ax,0
        jne     out1            ;done yet?
        cmp     cx,0
        je      sendnum
        push    cx 
sendnum: pop ax                 ;was dx
        IF DEBUG                ;number goes to serial port if in debug
        cmp     bugflg,2
        jne     normal
        cell    sertty
        jmp     normal2
        ;mov ah,2
        ;int 21h
normal:
        ENDIF
        call    clrtty
normal2: cmp    al,' ' ;uas dx
        jne sendnum
        IF DEBUG
        cmp     bugflg,2
        jne     qtnum
        dec     bugflg
        ENDIF
qtnum:  pop di
        ret
outnum endp

;******************************************************************************
RET_CD  EQU     0               ;ERRORLEVEL RETURN CODE VALUE
RET_FN  EQU     4CH             ;“RETURN TO DOS" FUNCTION CALL

        MOV     AX,RET_FN*256 + RET_CD  ;RETURN TO DOS FUNCTION CALL, AND
                                        ;VALUE TO BE PASSED T0 ERRORLEVEL
        INT     21H                     ;RETURN TO DOS
                                        ; (VERSION 2.00 OR LATER)
ENTPT ENDP

;******************************************************************************
;Initial Dictionary..
; The llink label should hang on the last initial link & edic should be
; the last thing defined.
; HEADERLESS HORDS
        EVEN                    ;A mini secondary that runs program
        IF debug
exe     db      5,'exec '       ;header is only for debug
exel    dw      0
        ENDIF
exec    dw      0               ;place for ptr to word to execute
        dw      goback          ;exit point for outer interpreter
goback  dw endrun

        EVEN
        IF debug
litter  db      3,'num '
litterl dw      0
        ENDIF
litterca dw     litterc
litterc: lodsu                  ;get next word in secondary
        xchg    sp,di           ;put it on the data stack
        push    ax
        xchg    sp,di
        jmp     next
        
        EVEN
        IF debug
dlitt   db      4,'dnum '
dlittl  dw      0
        ENDIF
dlittca dw      dlittc
dlittc: lodsu
        mov     dx,ax
        lodsu   
        xchg    sp,di
        push    ax
        push    dx
        xchg    sp,di
        jmp     next

        EVEN
        IF DEBUG
tron    db      4,'TRON '       ;set trace flag
tronl   dw      0
tronca  dw      offset tronc
tronc:  mov     bugflg,1
        jmp     next
troff   db      5,'TROFF'
trofl   dw      tronl
troFca  dw      offset trofc
trofc:  mov     bugflg,0
        jmp next
deep    db      4,'DEEP '       ;nesting level variable
deepl   dw      trofl
deepca  dw      deepc
deepc: xchg     sp,di
        mov     ax,offset deepu
        push    ax
        xchg    sp,di
        jmp     next
        ENDIF

        EVEN
one     db      3,'0NE '
        IF DEBUG
onel    dw      deepl           ;chain to trof if debug is set
        ELSE
onel    dw      0               ;last link if not debug...
        ENDIF
oneca   dw      offset onec
onec:   push    si
        mov     si,offset msg1
        call    messout
        pop     si
        jmp     next
        EVEN

man     db      10,'MANGI'
manl    dw      onel
manca   dw      offset colon
        dw      oneca
        dw      oneca
        dw      semi

tic     db 1,"' '
ticl    dw      manl
ticca   dw      ticc
ticc:   push    di              ;di has forth data stack ptr - token trashes
        call    token           ;tokenize the input
        call    search          ;call search
        ;jne nofnder ;***** UNFININSHED !!!!! *****

EVEN
literal db      128+7,'LITER'
literall dw     manl
literca dw      literc
literc: xchg    sp,di
        mov     bx,dp
        mov     [bx],offset litterca
        inc2    bx
        pop     [bx]
        inc2    bx
        mov     dp,bx
        xchg    sp,di
        jmp     next

        EVEN
dliteral db     128+8,'DLITE'
dliterll dw     Literall
dliterca dw     dliterc
dliterc: xchg   sp,di
        mov     bx,dp
        mov     [bx],offset dlittca
        inc2    bx
        pop     [bx]
        inc2    bx
        pop     [bx]
        inc2    bx
        mov     dp,bx
        xchg    sp,di
        jmp     next

        EVEN
dot     db      1,‘. '
dotl    dw      dliterll
dotca   dw      dotc
dotc:   xchg    sp,di
        pop     ax
        cmp     sp,topstk
        ja      stkerr
        xchg    sp,di
        call    outnum
        jmp     next
stkerr: mov     sp,di           ;back to system stack
        mov     di,topstk       ;reset data stack
        push    si
        mov     si,offset stkmsg
        call    messout
        pop     si
        jmp     next
        EVEN
paren   db      1+80h,'( '      ;scan to next parenthesis
parenl  dw      dotl            ;input stream ( comment )
parenca dw      parenc
parenc: call    getchar
        cmp     al,')'
        jne     parenc
        jmp     next

EVEN
udot    db      2,'U. '
udotl   dw      parenl
udotca  dw      udotc
udotc:  mov     uns,1           ;dot except set & reset unsigned flag
        xchg    sp,di
        pop     ax
        cmp     sp,topstk
        ja      stkerr
        xchg    sp,di
        call    outnum
        mov     uns,0
        jmp     next

EVEN
gpoun   db      2,'<# '
gpounl  dw      udotl
gpounca dw      gpounc
gpounc: mov     numptr,offset escratch+1
        jmp next
numptr  dw      0

EVEN
pound   db      1,'# '
poundl  dw      gpounl
poundca dw      poundc
poundc: xchg    sp,di
        mov     ax,radix
        push    ax              ;stack double base on top of number
        xor     al,al
        push    ax
        xchg    sp,di           ;div32 expects system stack on entry
        call    div32           ;divide # by base
        xchg    sp,di
        pop     cx              ;get high remainder
        pop     bx              ;get low remainder & make ascii
        mov     al,numset[bx]
        dec     numptr
        mov     bx,numptr       ;save in output string
        mov     [bx],al
        xchg    sp,di
        jmp next

EVEN
pounds  db      2,'#S '
poundsl dw      poundl
poundsca dw     poundsc
poundsc: xchg   sp,di
        mov     ax,radix
        push    ax              ;stack double base on top of number
        xor     ax,ax
        push    ax
        xchg    sp,di           ;div32 expects system stack
        call    div32           ;divide # by base
        xchg    sp,di
        pop     cx              ;get high remainder
        pop     bx              ;get low reainder & make ascii
        mov     al,numset[bx]
        dec     numptr
        mov     bx,numptr       ;save in output string
        mov     Ebx],al
        pop     ax              ;get the # of times
        pop     dx
        push    dx
        push    ax
        xchg    sp,di
        cmp     dx,0
        jne     poundsc
        cmp     ax,0
        jne     poundsc
        jmp     next

        EVEN
hold    db      4,'HOLD '
holdl   dw      poundsl
holdca  dw      holdc .
holdc:  xchg    sp,di
        pop     ax
        dec     numptr
        mov     bx,numptr
        mov     [bx],al
        xchg    sp,di
        jmp     next
        sign    db 4,'SIGN '
signl   dw      holdl
signca  dw      signc
signc:  xchg    sp,di
        pop     bx
        pop     cx
        pop     ax
        pop     cx
        pop     bx
        cmp     ax,0
        jge     exsign
        dec     numptr
        mov     bx,numptr
        mov     byte ptr [bx],'-'
exsign: xchg    sp,di
        jmp next

        EVEN
poundg db 2,'#>
poundgl dw signl
poundgca dw poundgc
poundgc: xchg sp,di
        pop bx                  ;toss the rest of the number (if any)
        pop bx
        mov bx,numptr
        push bx
        sub bx,offset escratch
        neg bx
        inc bx
        push bx
        xchg sp,di
        jmp next

        EVEN
dmod32  db      5,'D/MOD‘
dmod32l dw      poundgl
dmod32ca dw     dmod32c
dmod32c: call   div32
        jmp     next
div32:  xchg sp,di
        xor     bx,bx
        xor     cx,cx
        pop     divsh           ; cx:bx dx:ax
        pop     divsl           ; divsh:divsl
        pop     dx
        pop     ax
        push    di              ;I need another register!
        mov     divcnt,32
        clc
shft1:  rcl     ax,1            ;shift the 64 bit num 1 left
        rcl     dx,1
        rcl     bx,1
        rcl     cx,1
        mov     bp,bx           ;save cx:bx to eliminate add—back
        mov     di,cx
        sub     bx,divsl
        sbb     cx,divsh
        cmc                     ;reverse the carry (gives divided?)
        jb      shftck
        mov     bx,bp           ;it didn't go, restore cx:bx
        mov     cx,di
shftck: dec     divcnt
        jne     shft1
        rcl     ax,1            ;final shift of answer
        rcl     dx,1
        pop     di
        push    ax
        push    dx
        push    bx
        push    cx
        xchg    sp,di
        ret
divsl   dw      0
divsh   dw      0
divcnt  dw      0

        EVEN
coln    db      1,‘: '
colnl   dw      dmod32l
colnca  dw      colnc
colnc:  inc     mode            ;set compile mode on
        push    di              ;destroyed by token
        call    token
        mov     bx,current      ;get address of current toplink ptr
        mov     context,bx      ;set CONTEXT=CURRENT
        mov     ax,[bx]         ;get top link
        mov     [di+6],ax       ;pt new word to old top
        mov     ax,offset colon ;embed colon's address
        mov     [di+8],ax
        mov     bx,dE           ;save the dictionary ptr
        mov     odp, x          ; in case of error in compile
        lea     ax,[di+10]      ;move past link word
        mov     dp,ax           ;advance ram dp ptr for now
        pop     di
        jmp     next

        EVEN
sem     db      1+80h,'; '
seml    dw      colnl
semca   dw      semc
semc:   mov     ax,odp          ;get the old top
        add     ax,6            ;calculate 6 above old top
        mov     bx,current      ;get the address of current
        mov     lbx],ax         ;pt link ptr to new word
        mov     bx,ax           ;**** inefficiency?.. rework it!!!
        xor     ax,ax
        mov     mode,al         ;return to execute mode
        tobxdp  <offset semi>
        bxtodp                  ;set new top
        jmp     next

        EVEN
brack   db      1+80h,'[ '      ;word to set run-mode till 1
brackl  dw      seml
brackca dw      brackc
brackc: mov     mode, 0         ;set compile off
        ;special clean-up here?
        jmp next

        EVEN
rbrac   db      1+80h,'] '      ;will have no effect in compile mode
rbracl  dw      brackl
rbracca dw      rbracc
rbracc: inc     mode            ;turn compile mode on...
        jmp     next

        EVEN
create  db      6,'CREAT'
createl dw      rbracl
createca dw     createc
createc: push   di
        call    token
        mov     bx,current
        mov     context,bx      ;set search to vocab where new word is
        mov     ax,[bx]         ;get top link
        lea     di,[di+6]
        mov     [di],ax         ;pt new word to old top
        mov     [bx],di         ;link in link ptr
        inc     di              ;advance past link
        inc     di
        mov     ax,offset creatc ;get ptr to run-time code
        mov     [di],ax         ;place it in code address
        inc     di              ;advance dp
        inc     di
        mov     dp,di           ;update dictionary top
        pop     di
        jmp     next
        IF debug

        EVEN
creat   db      5,'creat'       ;should we add an extra inc here??!!!
creatl  dw      0               ; for the <builds does> thing...
creatca dw      creatc
        ENDIF
creatc: inc     ax              ;pt to word past code address
        inc     ax
        xchg    sp,di
        push    ax              ;push it to data stack
        xchg    sp,di
        jmp     next

EVEN
comma   db      1,‘, '
commal  dw      createl
commaca dw      commac
commac: xchg    sp,di
        mov     bx,dp
        pop     [bx]
        inc     bx
        inc     bx
        mov     dp,bx
        xchg    sp,di
        jmp     next
ccomma  db      2,'C, '
ccommal dw      commal
ccommaca dw     cconmac
ccommac: xchg   sp,di
        mov     bx,dp
        pop     ax
        mov     [bx],al
        inc     bx
        mov     dp,bx
        xchg    sp,di
        jmp     next

        EVEN
does    db      5+80H,'DOES>'
doesl   dw      ccommal
doesca  dw      doesc
doesc:  tobxdp  <offset idoesca>
        todp    Oe800h
        mov     ax,offset share - 2     ;calculate relative distance for call
        sub     ax,bx
        todp    ax
        bxtodp
        jmp     next

        EVEN
idoes   db      5,'does>'
idoesl  dw      doesl
idoesca dw      idoesc
idoesc: mov     bx,current      ;get the link address of the word
        mov     bx,[bx]
        inc2    bx
        inc     si              ;advance past 00 in the 00 & call
        mov     [bx],si         ;pt ca to share (colon-like word)
        jmp     semi            ;causes end of execution of word
                                ;this is compiled into before encounter
                                ;of the shared code which follows
share:  pop     bx              ;get the address passed the call that came here
        push    si              ;colon-like primitive that executes shared
        inc2    ax              ;the data area is the next word

        xchg    sp,di           ;switch to data stack
        push    ax              ;push it to the data stack
        xchg    sp,di
        mov     si,bx           ;get address of secondary code stacked a call
        nxtrun
EVEN
semcod  db      8,';CODE'
semcodl dw      doesl
semcodca dw     semcodc
semcodc: mov    ax,odp          ;do a "semi" first
        add     ax,6
        mov     bx,current      ;link in new word
        mov     [bx],ax
        mov     bx,ax           ;inefficient ????
        xor     ax,ax
        mov     mode,al         ;return to execute mode
        tobxdp  <offset scodeca> ;compile scode as the last word
        bxtodp
        jmp     next

        EVEN
        IF DEBUG
scode   db      5,'scode'
scodel  dw      0
        ENDIF
scodeca dw      scodec
scodec: mov     bx,current      ;get the link address of the word
        mov     [bx],bx
        inc2    bx
        mov     [bx],si         ;si pts to code space after scode
                                ; in the defining word
        jmp     semi            ;end secondary without a "semi" secondary
                                ;(causes a jump to the machine code portion)
        EVEN
forth   db      5,'FORTH'
forthl  dw      semcodl
forthca dw      forthc
forthc: mov     ax,offset fvoc
        mov     context,ax
        jmp     next

        EVEN
defs    db      11,'DEFIN'
defsl   dw      forthl
defsca  dw      defsc
defsc:  mov     ax,context
        mov     current,ax
        jmp     next

        EVEN
curnt   db      7,'CURRE'
curntl  dw      defsl
curntca dw      curntc
curntc: mov     ax,offset current
        xchg    sp,di
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
cont    db      7,'CONTE'
contl   dw      curntl
contca  dw      contc
contc:  mov      ax,offset context
        xchg    sp,di
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
const   dB      8,'CONST'
constl  dw      contl
constca dw      colon
constc  dw      createca
        dw      commaca
        dw      scodeca
        xchg    sp,di
        inc2    bx              ;pt to word past this code address that
        push    [bx]            ;& read the embedded data 
        xchg    sp,di           ; to copy to the stack
        jmp     next

EVEN
var     db      8,'VARIA'
varl    dw      constl
varca   dw      colon
varc    dw      createca        ;create will link in creat to push address
        dw      commaca         ;comma copies the default value...
        dw      semi            ;that's itH

        EVEN
at      db      1,'@    '
atl     dw      varl
atca    dw      atc
atc:    xchg    sp,di           ;replace top of stack with what it pts to
        pop     bx
        push    [bx]
        xchg    sp,di
        jmp     next

        EVEN
cat     db      2,'C@   '
catl    dw      atl
catca   dw      catc
catc:   xchg    sp,di
        pop     bx
        xor     ax,ax
        mov     al,[bx]
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
store   db      1,‘! '
storel  dw      catl
storeca dw      storec
storec: xchg    sp,di
        pop     bx              ;get the address
        pop     [bx]            ; & put the data in it
        xchg    sp,di
        jmp     next

        EVEN
cstore  db      2,'C! '
cstorel dw      storel
cstoreca dw     cstorec
cstorec: xchg   sp,di
        pop     bx
        pop     ax
        mov     [bx],al
        xchg    sp,di
        jmp     next

fill    db      4,'FILL '       ;this routine is definitely ineficient...
filll   dw      cstorel         ;should use a string op.
fillca  dw      fillc
fillc:  xchg    sp,di
        pop     ax              ;fill value
        pop     cx              ;number to fill
        pop     bx              ;starting address
        cmp     cx,0            ;fill nothing?
        je      flext
flp:    mov     [bx],al
        inc     bx
        loop    flp
flext:  xchg    sp,di 
        jmp     next

EVEN
base    db      4,'BASE '
basel   dw      filll
baseca  dw      basec
basec:  xchg    sp,di
        mov     ax,offset radix
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
decim   db      7,'DECIM'
deciml  dw      basel
decimca dw      decimc
decimc: mov     radix,10
        jmp     next

EVEN
dpl     db      3,'DPL '
dpll    dw      deciml
dplca   dw      dplc
dplc:   xchg    sp,di
        mov     ax,offset dblflg
        push    ax
        xchg    sp,di
        jmp     next

EVEN
cursor  db      6,'CURSO'
cursorl dw      dpll
cursorca dw     cursorc
cursorc: xchg   sp,di
        mov     ax,syscrsr
        xor     bx,bx
        xchg    bl,ah
        push    bx
        push    ax
        xchg    sp,di
        jmp     next

EVEN
mydup   db      3,'DUP '
mydupl  dw      cursorl
mydupca dw      mydupc
mydupc: xchg    sp,di
        pop     ax
        push    ax
        push    ax
        xchg    sp,di
        jmp     next

EVEN
drop    db      4,'DROP '
dropl   dw      mydupl
dropca  dw      dropc
dropc:  inc2    di
        jmp     next

EVEN
swap    db      4,'SwAP '
suapl   dw      dropl
suapca  dw      swapc
suapc:  xchg    sp,di
        pop     ax
        pop     bx
        push    ax
        push    bx
        xchg    sp,di
        jmp     next

EVEN
over    db      4,'0VER '
overl   dw      swapl
overca  dw      overc
overc:  xchg    sp,di           ;examine for speed rewrite !!
        pop     bx
        pop     ax              ;Znd from top
        push    ax
        push    bx
        push    ax
        xchg    sp,di
        jmp     next

EVEN
rot     db      3,'ROT '
rotl    dw      overl
rotca   dw      rotc
rotc:   xchg    sp,di
        pop     ax
        pop     bx
        pop     cx
        push    bx
        push    ax
        push    cx
        xchg    sp,di
        jmp     next

EVEN
pick    db      4,'PICK '       ;good example of top replacment...
pickl   dw      rotl
pickca  dw      pickc
pickc:  xchg    sp,di
        pop     cx              ;get depth to bring up
        mov     bx,sp
        dec     cx
        shl     cx,1            ;*2
        add     bx,cx
        mov     ax,ss:[bx]      ;get picked value
        push    ax
        xchg    sp,di
        jmp     next

EVEN
roll    db      4,'ROLL '       ;first pick
rolll   dw      pickl
rollca  dw      rollc
rollc:  xchg    sp,di           ;pick as above first...
        pop     cx
        mov     bp,cx
        mov     bx,sp
        dec     cx
        shl     cx,1
        add     bx,cx
        mov     ax,ss:[bx]
        push    ax
        mov     sp,di
        mov     dx,si           ;save si!
        mov     si,bx
        dec2    si
        mov     di,bx           ;di shoutd end up where it belongs...
        mov     ax,ss
        mov     es,ax           ;moving from stack seg
        mov     ds,ax           ;to stack seg
        mov     cx,bp           ;restore count
        std                     ;go from high to low
        repe     movsu
        cld                     ;set it back for forth
        mov     ds,cs:savds
        mov     si,dx           ;restore si
        inc2    di              ;takes care of final dec in repe
        jmp     next
        
        EVEN
qdup    db      4,'?DUP '
qdupl   dw      rolll
qdupca  dw      qdupc
qdupc:  xchg    sp,d1
        pop     ax
        push    ax
        cmp     ax,0
        je      qqdup
        push    ax
qqdup:  xchg    sp,di
        jmp     next
        
        EVEN
depth   db      5,'DEPTH'
depthl  dw      qdupl
depthca dw      depthc
depthc: xchg    sp,di
        mov     cx,topstk
        sub     cx,sp
        shr     cx,1
        push    cx
        xchg    sp,di
        jmp     next

        EVEN
h       db      1,'H    '
hl      dw      depthl
hca     dw      hc
hc:     xchg    sp,di
        mov     ax,offset dp
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
here    db      4,'HERE '
herel   dw      hl
hereca  dw      herec
herec:  xchg    sp,di
        push    dp
        xchg    sp,di
        jmp     next

        EVEN
gr      db      2,'>R   '
grl     dw      herel
grlca   dw      grlc
grlc:   xchg    sp,di
        pop     ax
        xchg    sp,di
        push    ax
        jmp     next

        EVEN
rg      db      2,'R>   '
rgl     dw      grl
rglca   dw      rglc
rglc:   pop     ax
        xchg    sp,di
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
rat     db      2,'R@   '
ratl    dw      rgl
ratca   dw      ratc
ratc:   pop     ax
        push    ax
        xchg    sp,di
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
lt      db      1,'< '          ;good example of 1 for 2
ltl     dw      ratl
ltca    dw      ltc
ltc:    xor     ax,ax
        xchg    sp,di
        pop     bx
        pop     cx
        cmp     cx,bx
        jge     pshf
        dec     ax              ;changed from Inc to give -1=true
pshf:   push    ax
        xchg    sp,di
        jmp     next

        EVEN
eq      db      1,'=    '
eql     dw      ltl
eqca    dw      eqc
eqc:    xor     ax,ax
        xchg    sp,di
        pop     bx
        pop     cx
        cmp     cx,bx
        jne     pshf1
        dec     ax
pshf1:  push    ax 
        xchg    sp,di
        jmp     next

        EVEN
gt      db      1,'>    '
gtl     dw      eql
gtca    dw      gtc
gtc:    xor     ax,ax
        xchg    sp,di
        pop     bx
        pop     cx
        cmp     cx,bx
        jle     pshf2
        dec     ax
pshf2:  push    ax
        xchg    sp,di
        jmp     next

EVEN
lt0     db      2,'0<   '
lt0l    dw      gtl
lt0ca   dw      lt0c
lt0c:   xor     ax,ax
        xchg    sp,di
        pop     bx
        cmp     bx,ax
        jge     lt0e
        dec     ax
lt0e:   push    ax
        xchg    sp,di
        jmp     next

EVEN
eq0     db      2,'0=   '
eq0l    dw      lt0l
eqOca   dw      eq0c
eq0c:   xor     ax,ax
        xchg    sp,di
        pop     bx
        cmp     bx,ax
        jne     eq0e
        dec     ax
eq0e:   push    ax
        xchg    sp,di
        jmp     next

EVEN
gr0     db      2,'0>   '
gr0l    dw      eq0l
gr0ca   dw      gr0c
gr0c:   xor     ax,ax
        xchg    sp,di
        pop     bx
        cmp     bx,ax
        jle     gr0e
        dec     ax
gr0e:   push    ax
        xchg    sp,di
        jmp     next

EVEN
unscmp  db      2,'U<   '
unscmpl dw      gr0l
unscmpca dw     unscmpc
unscmpc:xor     ax,ax
        xchg    sp,di
        pop     bx
        pop     cx
        cmg     cx,bx
        jnb     unscmpe
        dec     ax
unscmpe: push   ax
        xchg    sp,di
        jmp     next

        EVEN
not     db      3,'NOT  '
notl    dw      gr0l
notlca  dw      notlc
notlc:  xor     ax,ax
        xchg    sp,di
        pop     bx
        cmp     ax,bx
        jne     enot
        dec     ax
enot:   push    ax
        xchg    sp,di
        jmp     next

        EVEN
and     db      3,'AND  '
andl    dw      notl
andlca  dw      andlc
andlc:  xchg    sp,di
        pop     ax
        pop     bx
        and     ax,bx
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
or      db      2,'OR   '
orl     dw      andl
orlca   dw      orlc
orlc:   xchg    sp,di
        pop     ax
        pop     bx
        or      ax,bx
        push    ax
        xchg    sp,di
        jmp     next

EVEN
xor     db      3,'XOR  '
xorl    dw      orl
xorca   dw      xorc
xorc:   xchg    sp,di
        pop     ax
        pop     bx
        xor     ax,bx
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
plus    db      1,'+    '
plusl   dw      xorl
plusca  dw      plusc
plusc:  xchg    sp,di
        pop     ax
        mov     bx,sp
        add     ss:[bx],ax
        xchg    sp,di
        jmp     next

        EVEN
pstor   db      2,'+!   '
pstorl  dw      plusl
pstorca dw      pstorc
pstorc: xchg    sp,di
        pop     bx
        pop     ax
        add     [bx],ax
        xchg    sp,di
        jmp     next

        EVEN
sub     db      1,'-    '
subl    dw      pstorl
subca   dw      subc
subc:   xchg    sp,di
        pop     ax
        mov     bx,sp
        sub     ss:[bx],ax
        xchg    sp,di
        jmp     next

        EVEN
times   db      1,'*    '
timesl  dw      subl
timesca dw      timesc
timesc: xchg    sp,di
        pop     cx
        pop     ax
        imul    cx
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
tdiv    db      2,'*/   '
tdivl   dw      timesl
tdivca  dw      tdivc
tdivc:  xchg    sp,di
        pop     cx
        pop     ax
        imul    cx
        pop     cx
        idiv    cx
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
tdmod   db      2,'*/MOD‘
tdmodl  dw      tdivl
tdmodca dw      tdmodc
tdmodc: xchg    sp,di
        pop     cx
        pop     ax
        imul    cx
        pop     cx
        idiv    cx
        push    dx
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
utimes  db      2,'U*   '
utimesl dw      tdmodl
utimesca dw     utimesc
utimesc: xchg   sp,di
        pop     cx
        pop     ax
        mul     cx
        push    ax

        EVEN
div     db      1,'/    '
divl    dw      utimesl
divca   dw      divc
divc:   xchg    sp,di
        pop     cx
        pop     ax
        cwd                     ;extend ax to dx
        idiv    cx
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
imod    db      3,'MOD '
imodl   dw      divl
imodca  dw      imodc
imodc:  xchg    sp,di
        pop     cx
        pop     ax
        cwd
        idiv    cx
        push    dx
        xchg    sp,di
        jmp     next

        EVEN
dmod    db      4,‘/MOD '
dmodl   dw      imodl
dmodca  dw      dmodc
dmodc:  xchg    sp,di
        pop     cx
        pop     ax
        cwd
        idiv    cx
        push    dx
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
plus1   db      2,'1+   '
plus1l  dw      dmodl
plus1ca dw      plus1c
plus1c: inc     word ptr ss:[di]
        jmp     next

        EVEN
minus1  db      2,'1-   '
minus1l dw      plus1l
minus1ca dw     minus1c
minus1c: dec    word ptr ss:[di]
        jmp     next 

        EVEN
plus2   db      2,'2+   '
plus2l  dw      minus1l
plus2ca dw      plus2c
plus2c: mov     ax,2
        add     ss:[di],ax
        jmp     next

        EVEN
minus2  db      2,'2-   '
minus2l dw      plus2l
minus2ca dw     minus2c
minus2c: mov    ax,2
        sub     ss:[di],ax
        jmp     next

        EVEN
dpls    db      2,'D+   '
dplsl   dw      minus2l
dplsca  dw      dplsc
dplsc:  xchg    sp,di
        pop     ax
        pop     dx
        pop     bx
        pop     cx
        add     dx,cx
        adc     ax,bx
        push    dx
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
max     db      3,'MAX  '
maxl    dw      dplsl
maxca   dw      maxc
maxc:   xchg    sp,di
        pop     ax
        pop     bx
        cmp     ax,bx
        jg      psha
        push    bx
        xchg    sp,di
        jmp     next
psha:   push    ax
        xchg    sp,di
        jmp     next

        EVEN
min     db      3,'MIN  '
minl    dw      maxl
minca   dw      minc
minc:   xchg    sp,di
        pop     ax
        pop     bx
        emp     ax,bx
        jg      pshb
        push    ax
        xchg    sp,di
        jmp     next
pshb:   push    bx
        xchg    sp,di
        jmp     next

        EVEN
abs     db      3,'ABS  '
absl    dw      mint
absca   dw      absc
absc:   xchg    sp,di
        pop     ax
        cmp     ax,0
        jl      ngt             ;borrow negate's negate...
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
negate  db      6,'NEGAT“
negatel dw      absl
negateca dw     negatec
negatec: xchg   sp,di
        pop     ax
ngt:    neg     ax
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
dnegat  db      7,'DNEGA'
dnegatl dw      negatel
dnegatca dw     dnegatc
dnegatc: xchg   sp,di
        pop     dx
        pop     ax
        xor     ax,Offffh       ;reverse the bits
        xor     dx,0ffffh       ; (1s compl.)
        add     ax,1            ;now for 2's compl.
        adc     dx,0            ;copy over the carry
        push    ax
        push    dx
        xchg    sp,di
        jmp     next

        EVEN
allot   db      5,'ALLOT'
allotl  dw      dnegatl
allotca dw      allotc
allotc: xchg    sp,di
        pop     cx              ;pop the data stack
        xchg    sp,di
        shl     cx,1
        add     dp,cx           ;add that many to the dictionary
        jmp     next

        EVEN
begin   db      128+5,'BEGIN'
beginl  dw      allotl
beginca dw      beginc
beginc: xchg    sp,di           ;store top of dictionary to stack
        push    dp
        xchg    sp,di
        jmp     next

        EVEN
eend    db      128+3,'END '
eendl   dw      beginl
eendca  dw      eendc
eendc:  xchg    sp,di
        tobxdp  <offset indca>  ;get the address of send rtn
        pop     [bx]            ;and place it after send in the dictionary
        inc2    bx
        bxtodp
        xchg    sp,di
        jmp     next

        EVEN
ind     db      5,'end  '
indl    dw      0
indca   dw      indc
indc:   xchg    sp,di
        pop     ax
        xchg    sp,di
        cmp     ax,0
        jne     sadv            ;if equal exit the loop
        fjump                   ;forth jup to word pointed to by si

sadv: skip

        EVEN
until   db      5+128,'UNTIL'   ;synonym for end
untill  dw      eendl
untilca dw      eendc

        EVEN
iif     db      128+2,'IF   '
iifl    dw      untill
iifca   dw      iifc
iifc:   xchg    sp,di
        tobxdp  <offset iiifca> ;get code address if internal code
        push    bx              ;push dp for next or else
        inc2    bx
        bxtodp                  ;store the dp
        xchg    sp,di
        jmp     next

        EVEN
        IF debug
iiif    db      2,'if '
iiifl   dw      O
        ENDIF
iiifca  dw      iiifc
iiifc:  xchg    sp,di
        pop     ax              ;pop data stack
        xchg    sp,di
        cmp     ax,0            ;true or false?
        jne     iftru
        fjump
iftru: skip

        EVEN
then    db      128+4,'THEN '
thenl   dw      iifl
thence  dw      thenc           ;NOTE: AX IS THE DP IN THIS ROUTINE!
thenc:  xchg    sp,di
        mov     ax,dp           ;get the dp
        pop     bx              ;pop the address of the if jump address
        mov     [bx],ax         ;so He can store the word after in it
        xchg    sp,di
        jmp     next

eelse   db      128+4,'ELSE '
elsel   dw      thenl
elseca  dw      elsec
elsec:  xchg    sp,di
        tobxdp  <offset ielseca> ;add ielse to the dictionary
        pop     ax               ;get the if destination from the stack
        xchg    ax,bx
        push    ax
        inc2    ax
        mov     [bx],ax
        mov     dp,ax           ;restore dp
        xchg    sp,di
        jmp     next

        EVEN
        IF debug
ielse   db      5,'else '
ielsel  dw      0
        ENDIF
ielseca dw      ielsec
ielsec: mov     si,[si]         ;go to the THEN
        lodsw                   ;next & run
        mov     bx,ax
        IF debug
        call    ckbug
        ENDIF
        jmp     [bx]

        EVEN
do      db      2+80h,'DO   '
dol     dw      elsel
doca    dw      doc
doc:    tobxdp  <offset idoca>
        bxtodp
        xchg    sp,di
        push    bx
        xchg    sp,di
        jmp     next

ido     db      2,'do   '
idol    dw      0
idoca   dw      idoc
idoc:   xchg    sp,di
        pop     ax              ;get the start count from data stack
        pop     bx              ;... end count
        xchg    sp,di
        push    bx
        push    ax
        jmp     next

        EVEN
loop    db      4+80h,'LOOP '
loopl   dw      dol
loopca  dw      loopc
loopc:  xchg    sp,di
        pop     ax              ;return address from do
        xchg    sp,di
        tobxdp  <offset iloopca>
        todp    ax
        bxtodp
        jmp     next

        EVEN
iloop   db      4,'loop '
iloopl  db      0
iloopca dw      iloopc
iloopc: pop     ax              ;start (index)
        inc     ax
        pop     bx
        cmp     bx,ax
        jle     exloop          ;count is still < end value so...
        push    bx              ;replace the indices
        push    ax
        fjump
exloop: skip

        EVEN
i       db      1,'I    '
il      dw      loopl
ilca    dw      ilc
ilc:    pop     ax
        push    ax
        xchg    sp,di
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
crc     db      2,'CR   '
crcl    dw      it
crcca   dw      crcc
crcc:   mov     cx,si
        mov     si,offset nferr+1
        call    messout
        mov     si,cx
        jmp     next

        EVEN
attrib  db      9,'ATTRI'       ;page*256+attribut ATTRIBUTE
attribl dw      crcl
attribca dw     attribc
attribc: xchg   sp,di
        mov     ax,offset attribute
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
emit    db      4,'EMIT ' ;
emitl   dw      attribl
emitca  dw      emitc
emitc:  xchg    sp,di
        pop     ax
        xchg    sp,di
        call    clrtty
        jmp     next

        EVEN
ead     dw      0
typ     db      4,'TYPE '
typel   dw      emitl
typeca  dw      typec
typec:  xchg    sp,di
        pop     cx              ;get count
        pop     bx              ;get ptr to the output string
        cmp     cx,0            ;if count is <=0 just return
        jle     extype
typelp: mov     al,[bx]
        call    clrtty
        inc     bx
        loop    typelp
extype: xchg    sp,di
        jmp     next

        EVEN
zspace  db      5,'SPACE'
spacel  dw      typel
spaceca dw      spacec
spacec: mov     al,' '
        call    clrtty
        jmp     next
        
        EVEN
spaces  db      6,'SPACE'
spacesl dw      spacel
spacesca dw     spacesc
spacesc: xchg   sp,di
        pop     cx
        xchg    sp,di

dspc:   dec     cx
        jl      exspc
        mov     al,' '
        call    clrtty
        imp     dspc
exspc:  jmp     next

prt     db      128+2,'."   '
prtl    dw      spacesl
prtca   dw      prtc
prtc:   mov     leadin,0        ;make spaces significan for token
        mov     delim,'"'       ;terminate with a quote
        mov     cvt,0           ;don't convert to upper
        push    di
        push    si
        cmp     mode,0
        jne     prtcomp
        call    token           ;returns with di at beginning
        mov     byte ptr [di+bx],0 ;wipe out token's extra space
        mov     si,di           ;of string
        inc     si              ;advance past number
        call    messout         ;echo to screen...
exprt:  pop     si
        pop     di
        mov     al,' '
        mov     delim,al        ;restore the delimiter & leadins
        mov     leadin,al       ; for the outer interpreter
        mov     cvt,32          ;return to upper only mode
        jmp     next

prtcomp: mov    cx,dp           ;get next free word
        inc     dp              ;this byte+the len from token
        call    token           ;will leave a hole for the CA
        mov     byte ptr [di+bx],0 ;wipe out token's extra space
        inc     bx              ;count the len byte
        add     bx,di           ;adjust dictionary past token
        bxtodp                  ;save it
        mov     bx,cx           ;put code address of runtime print
        mov     word ptr [bx],offset inprtca ; in dictionary
        jmp     exprt

inprt   db      5,'."lit'
inprtl  dw      0
inprtca dw      inprtc
inprtc: mov     al,[si]         ;the string follows the CA
        cmp     al,0
        je      exnin
        ;mov ah,2 ;write character function
        ;int 21h
        inc     si
        call    clrtty
        jmp     inprtc
exnin:  inc si                  ;mov to next word
        jmp next

EVEN
open    db      4,'OPEN '       ;mode " filname“ OPEN -> handle or -errornum
openl   dw      prtl
openca  dw      openc
openc: xchg     sp,di           ;get to stack for filename
        pop     dx              ;get the address of the new word (-1)
        inc     dx              ; the path for the dos call
        pop     ax              ;get the mode from the data stack
        mov     ah,3dh          ;open function code
        int     21h             ;call dos
        jae     noperr          ;no carry flag return handle
        neg     ax
noperr: push    ax              ;return error code/handle
        xchg    sp,di
        jmp     next

EVEN
getrec  db      6,'GETRE'       ;buffer ptr --> # read or -error
getrecl dw      openl           ;buffer = handle+buflen+datalen+data
getrecca dw     getrecc
getrecc: xchg   sp,di
        pop     bx              ;get the buffer pointer
        push    bx              ;we'll need it back
        mov     cx,[bx+2]       ;read buflen bytes
        lea     dx,[bx+6]       ;buffer for data
        mov     bx,[bx]         ;put the handle in bx
        mov     ah,3fh          ;read function
        int     21h             ;if no carry return byte as-is
        pop     bx              ;put len & status in datalen
        jae     ngeterr
        neg     ax              ;return negative error code
urtax:  push    ax
        mov     [bx+4],ax       ;also record it in the record
        xchg    sp,di
        jmp     next
        ngeterr: cmp ax,0
        jne     nof
        dec     ax              ;substitutute return of -1 for 0 on eof
        jmp     urtax
        nof:    push ax
        mov     [bx+4],ax       ;save status in record
        xchg    sp,di
        jmp     next

EVEN
putrec  db      6,'PUTRE'       ;buffer ptr --> # put or -error
putrecl dw      getrecl         ;buffer = handle+buflen+datalen+data
putrecca dw     putrecc
putrecc: xchg   sp,di
        pop     bx              ;get the buffer pointer
        push    bx              ;we'll need it back
        mov     cx,[bx+2]       ;put buflen bytes
        lea     dx,[bx+6]       ;buffer for data
        mov     bx,[bx]         ;put the handle in bx
        mov     ah,40h          ;read function
        int     21h             ;if no carry return byte as-is
        pop     bx              ;put len & status in datalen
        jae     nputerr
        neg     ax              ;return negative error code
urtax2: push    ax
        mov     [bx+4],ax
        xchg    sp,di
        jmp     next
nputerr: cmp    ax,0
        jne     nof2
        dec     ax              ;substitutute return of -1 for 0 on eof

        jmp     wrtax2
        nof2:   push ax
        mov     [bx+4],ax
        xchg    sp,di
        jmp     next

EVEN
seek    db      4,'SEEK '       ;seek file byte ( n1 ud1 n2 --> ud2 )
seekl   dw      putrecl         ; n1=seek mode (abs,crnt+offset,end+offst)
seekca  dw      seekc           ; ud1=seek position
seekc:  xchg    sp,di           ; n2=handle of file to seek in
        pop     bx              ; ud2=position after call
        pop     cx              ; if neg, high word has error num
        pop     dx
        pop     ax
        xchg    al,ah
        mov     ah,42h
        int     21h
        jae     noskerr
        neg     ax
        xor     bx,bx           ;if error return high byte as neg error num
        push    bx
        push    ax
        xchg    sp,di
        jmp     next
noskerr: push   ax
        push    dx
exseek: xchg    sp,di
        jmp     next

EVEN
getc    db      7,'GETCH'       ;file handle -> char
getcl   dw      seekl
getca   dw      getcc
getcc:  xchg    sp,di
        pop     bx              ;get the file handle
        mov     cx,1            ;read 1 byte
        mov     dx,offset tget  ;buffer for data
        mov     ah,3fh          ;read function
        int     21h             ;if no carry return byte as-is
        jae     ongeterr
        neg     ax              ;return negative error code
owrtax: push    ax
        xchg    sp,di
        jmp     next
ongeterr: cmp   ax,0
        jne     onof
        qec     ax              ;substitutute return of -1 for 0 on eof
        jmp     urtax
onof:   push    tget
        xchg    sp,di
        jmp     next
tget    dw      0               ;1 word buffer

EVEN
close   db      5,'CLOSE'
closel  dw      getcl
closeca dw      closec
closec: xchg    sp,di
        pop     bx
        mov     ah,3eh
        jb      ncler '
        xor     ax,ax
ncler:  push    ax
        xchg    sp,di
        jmp     next

EVEN
locate  db      6,'LOCAT'       ;set cursor position
locatel dw      closel          ;x y locate
locateca dw     locatec
locatec: xchg   sp,di
        pop     dx              ;get x (column)
        pop     ax              ;get y (row) through ax
        mov     dh,al
        mov     syscrsr,dx      ;let clrtty know about it!
        mov     bx,0            ;get display page
        mov     ah,2            ;service 2, set cursor
        push    bp              ;trashed by IBM BIOS??
        int     10h 
        pop     bp 
        xchg    sp,di
        jmp     next

EVEN
scroll  db      6,'SCROL'       ;n scroll up -n scroll down
scrolll dw      locatel
scrollca dw     scrollc
scrollc: xchg   sp,di
        pop     ax              ;get number to scroll
        call    scrollit
        xchg    sp,di
        jmp     next

        EVEN
load    db      4,'LOAD '       ; LOAD filename
loadl   dw      scrolll
loadca  dw      loadc
loadc:  xchg    sp,di
        pop     dx              ;get the address of the filename -1
        xchg    sp,di
        inc     dx
        xor     ax,ax           ;mode is read
        mov     ah,3dh          ;open function
        int     21h
        jb      lderr
        mov     rhandle,ax      ;save the handle for getchar
        mov     filin,1         ;set the "read from file" flag
        jmp     next 
lderr:  push    si              ;write the load error message
        mov     si,offset lderrmes
        call    messout
        pop     si
        jmp     next

quote   db      1+80h,'"    '   ;make a string literal in scrath pad
quotel  dw      loadl
quoteca dw      quotec
quotec: mov     leadin,0        ;make spaces significant to token
        mov     delim,'"'       ;terminate with quote
        mov     cvt,0
        push    di
        push    si
        cmp     mode,0          ;do we want to build the string in
        jne     qtcomp          ;scratch or program (based on mode)
        call    token
        mov     cx,bx           ;get len to cx
        mov     si,di           ;pt si at beginning
        mov     di,offset scratch
        mov     bx,di           ;to be pushed...
        cld
        repne   movsb

        mov     byte ptr [di],0 ;nul terminate for "C-like" functions
exqt:   pop     si
        pop     di
        mov     at,' '
        mov     delim,al
        mov     leadin,al
        mov     cvt,32
        xchg    sp,di
        push    bx
        xchg    sp,di
        jmp     next
qtcomp: mov     cx,dp
        inc2    dp
        call    token
        mov     ax,bx
        add     bx,di
        inc     bx              ;add 1 space for null termination
        bxtodp
        mov     bx,cx
        mov     word ptr [bx],offset rqtca
        pop     si
        pop     di
        mov     at,' '
        mov     delim,al
        mov     leadin,al
        mov     cvt,32
        jmp     next

        EVEN
rqt     db      5,'"lit '
rqtl    dw      0
rqtca   dw      rqtc
rqtc:   xchg    sp,di
        xor     ax,ax
        mov     al,[si]
        push    si
        add     si,ax
        inc2    si
        xchg    sp,di
        jmp     next

        EVEN
count   db      5,'COUNT'
countl  dw      quotel
countca dw      countc
countc: xchg    sp,di
        pop     bx
        xor     ax,ax
        mov     al,[bx]
        inc     bx
        push    bx
        push    ax
        xchg    sp,di
        jmp     next

        EVEN
ltrail  db      9,'-TRAI'
ltraill dw      countl
ltrailca dw     ltrailc
ltrailc: xchg   sp,di
        pop     bx
        pop     cx
        push    cx
        xchg    cx,di
        dec     di
traillp: cmp    bx,0
        je      extrail
        cmp     byte ptr [bx+di],' '
        jne     extrail
        dec     bx
        jmp     traillp
extrail: push   bx
        mov     di,cx
        xchg    sp,di
        jmp     next

        EVEN
key     db      3,'KEY '
keyl    dw      ltraill
keylca  dw      keylc
keylc:  xchg    sp,di
        mov     ah,0            ;service 1 - wait for key
        int     16h             ;bios keyboard int
        push    ax
        xchg    sp,di
        jmp     next
        
expect  db      6,'EXPEC'
expectl dw      keyl
expectca dw     expectc
expectc: xchg   sp,di
        mov     dx,di
        pop     cx              ;maximum # of chars to read
        pop     di              ;buffer address
        xor     bx,bx
exkey:  xor     ah,ah           ;0 = wait for key
        int     16h             ;bios keyboard int
        cmp     al,tab          ;ignore tabs
        je      exkey
        cmp     al,lf
        je      exkey
        cmp     al,cr
        je      exitk
        cmp     al,27
        je      exitk
        cmp     bx,cx           ;a end of buffer only backspace allowed
        jne     ckbs
        cmp     al,bs
        jne     exkey
ckbs:   cmp     al,bs           ;if backspace, fix buffer
        jne     exbak
        cmp     bx,0
        je      exkey
        mov     al,bs           ;back space char
        call    clrtty
        mov     al,' '          ;destroy char under cursor
        call    clrtty
        mov     al,bs           ;put cursor back
        call    clrtty
        cmp     bx,0
        je      exkey
        dec     bx              ;back up if not at margin
        mov     byte ptr [bx+di],' '
        jmp     exkey
exbak:  mov     byte ptr [bx+di],al
        call    clrtty
        inc     bx
        jmp     exkey
exitk:  xor     ah,ah
        mov     dblflg,ax       ;save termination char
exclr:  cmp     bx,cx
        jae     expitk2
        mov     byte ptr [bx+di],' ' ;clear to end of buffer
        inc     bx
        jmp     exclr
expitk2: mov    di,dx
        xchg    sp,di
        jmp     next

        EVEN
convrt  db      7,'CONVE'       ;convert & add string input double
convrtl dw      expectl
convrtca dw     convrtc
convrtc: push   si
        xchg    sp,di           ;bring in data stack
        xor     cx,cx
        mov     dblflg,cx       ;have cknum assume double-0 places
        mov     cvtmp,di
        pop     di              ;get string add & len
        mov     cl,[di]
        add     cx,di           ;get string end address
        mov     si,dp
        mov     bx,si
        mov     byte ptr [si],O ;zero the len byte
        inc     bx              ;pt to 1st actual char
        inc     di
ldlp:   mov     al,[di]         ;strip leading spaces
        cmp     al,' '
        jne     cpylp
        inc     d1
        cmp     cx,di
        jnb     ldlp            ;go back if not eos
        xor     ax,ax           ;all spaces is special case
        push    ax
        push    ax
        jmp     allblks
cpylp:  mov     al,[di]
        mov     [bx],al         ;copy char to output buffer
        cmp     al,' '
        je      excplp
        inc     byte ptr [s1]   ;inc len byte
        inc     di
        inc     bx
        cmp     cx,di
        jnb     cpylp
        mov     byte ptr [bx],' ' ;save terminator for cknum
excplp: call    cvnum           ;call cknum's alt. entry point
        pop     cx              ;add low of stack to low result
        add     ax,cx
        mov     dx,hisave
        pop     cx
        adc     dx,cx
        push    ax              ;save as a double
        push    dx
allblks: mov    di,cvtmp
        xchg    sp,di
        pop     si
        jmp     next
cvtmp   dw      0

        EVEN
quit    db      4,'QUIT '
quitl   dw      convrtl
quitca  dw      quitc
quitc:  mov     ax,ret_fn*256
        int 21h                 ;that's all folks

        EVEN
llink   equ     quitl
edic    dw      0               ;end of initial dictionary
free    dw      48 dup (256 dup (0,0))  ;should be 16 * (256*4=1k) bytes (?,?)
CSEG ENDS

;******************************************************************************
STACK   SEGMENT PARA STACK 'STACK'
        DB      TOPSTK DUP ("D")
        DB      120 DUP (“STACK “) ;240 byte system stack
STACK   ENDS
        END ENTPT

