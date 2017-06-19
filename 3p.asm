
dane1 	segment
handler	dw	?
buf	db	105 dup('$')
buffer db ?

parsed_arguments		db 127 dup(?)
length_of_args			db 50 dup(0)
LOG	db 	"Lack of arguments$"
TMA db "Too many arguments$"
WA db "Wrong arguments$"

float	dq	1.0	


P1 dq -2.0
P2 dq 1.0
P3 dq -1.0
P4 dq 1.0

wordBuffer		dw	?
qwordBuffer		dq	?

p				dq	?		
q				dq	?

TEN				dw	10d	
leftFloat		dw	?			
rightFloat		dw	?			
rightLen		dw	?	
MAX	dq 4.0


dane1	ends
code1	segment
.386
.386		


start1:	
	mov	ax,seg top1
	mov	ss,ax
	mov	sp,offset top1
	mov	ax,seg dane1
	mov	ds,ax
	call parse_arguments
	call check_arguments ;checking amount od args
	call convert_arguments_to_floats ;and checking if they are floats
	
	xor ah, ah
	mov al, 13d			;VQA in
	int 10h
	
	call showSet
	
	xor ax, ax
	int 16h
	
	xor ah, ah
	mov al, 3d			;VGA out
	int 10h


	jmp end_of_program_without_exception

	
	
	;;;;;;;;;;;;;;;;;;;;
	convert_arguments_to_floats proc
		xor cx, cx

		mov si, offset parsed_arguments ;pointer to firt
		mov cl, ds:[length_of_args] ;need to MakeFloat 
		call makeFloat
		fld qword ptr ds:[float] ;
		fstp qword ptr ds:[P1]    ; moving output to P1 by cop stack

		add si, cx
		inc si
		mov cl, ds:[length_of_args+1]
		call makeFloat

		fld qword ptr ds:[float] 
		fstp qword ptr ds:[P2]

		add si, cx
		inc si
		mov cl, ds:[length_of_args+2]
		call makeFloat

		fld qword ptr ds:[float]
		fstp qword ptr ds:[P3]

		add si, cx
		inc si
		mov cl, ds:[length_of_args+3]
		call makeFloat
		fld qword ptr ds:[float]
		fstp qword ptr ds:[P4]
		ret
	convert_arguments_to_floats endp
	
	makeFloat proc ;cl - length, si - source; ds:[floar] - output
			push ax
			push bx
			push dx	
			pushf

		    cmp cl, 3d					;minumimun 3 signs
			jb wrong_arguments
			mov ch, cl
			xor cl, cl							;CL - iterator
			
			xor ax, ax				;bumber as result of left or right loop
			xor dx, dx				;DL - 'minus' flag
			
			mov bl, byte ptr ds:[si]
			cmp bl, "-"
			jne loopLeft
			mov dl, 1d			;minus 
			inc si				;if minus then number starts from next sign
			inc cl				;iterator ++
			
			
		loopLeft:
			cmp cl, ch
			je wrong_arguments	;number cannot end before dot
			xor bx, bx			;bh=0;
			mov bl, byte ptr ds:[si]		;bl <-  ASCII
			cmp bl, "."			
			je foundDot
			cmp bl, "0"			
			jb wrong_arguments		
			cmp bl, "9"
			ja wrong_arguments		
			push dx ;just prevent not to destroy dx
			mul ds:[TEN]			;10
			pop dx
			sub bl, "0"			;ascii -> num
			add ax, bx			;and add to result
			inc si
			inc cl			;iterator++
			jmp loopLeft
			
		foundDot:

			mov word ptr ds:[leftFloat], ax		;left prt to memory
			inc si			;
			inc cl			;iterator ++
			
			cmp cl, 2d				;dot cannot be in pos >2
			jb wrong_arguments	
			cmp cl, ch				;and cannot be last sigh
			je wrong_arguments
			
			xor ax, ax		;res number of R loop
			xor bx, bx		;leghth of R part
			
		loopRight:			
			push bx			;but firtstly it will be used in another way
			
			xor bx, bx			; bh =0;
			mov bl, byte ptr ds:[si]		;same logic as in L
			cmp bl, "0"			
			jb wrong_arguments		
			cmp bl, "9"
			ja wrong_arguments		
			push dx			
			mul ds:[TEN]			
			pop dx
			sub bl, "0"			
			add ax, bx			
			inc si			
			pop bx			
			inc bx			;R length ++
			inc cl
			cmp cl, ch			;if end
		jb loopRight
			
		mov word ptr ds:[rightFloat], ax		;ax -> right side
		
		mov cx, bx			;divide bx timex right side
		
		fild ds:[leftFloat]		;st(2) L
		fild ds:[TEN]				;st(1) - 10.0
		fild ds:[rightFloat]		;st(0) - R
			
		makeQLoop:
			fdiv st(0), st(1)		;st(0):=st(0)/st(1) 
		loop makeQLoop
		
		fadd st(0), st(2)		;st(0):=st(0)+st(2)a
		
		cmp dl, 1d			;minus
		jne saveNum
		fchs			;st(0)=-st(0)
		saveNum:
			fcom ds:[MAX]
			fstsw ax		;FPU -> AX
			sahf			;zapisuje z AH do rejestru flag
			jna notTooGreat		;jesli st(0) > MAX to skacze do flagi - przerwanie
				jmp wrong_arguments
			notTooGreat:

			fstp qword ptr ds:[float]			;zapisuje st(0) - gotowa liczbe zmiennoprzecinkowa - do pamieci i zdejmuje ze stosu

			fsubp st(0), st(0)		;s(1)=0
			fsubp st(0), st(0)		;s(0)=0
			
			popf
			pop dx
			pop bx
			pop ax
			ret
	makeFloat endp	


	showSet proc
			pusha
			pushf
			
			xor cx, cx		;point (0,0)
			xor dx, dx
			
			mov ah, 0Ch		
		loopColumn:
			cmp dx, 200d
			je showEnd
			loopRow:
				cmp cx, 320d		;moving to the end of every row
				je endOfInnerLoop		;row ended

				mov ax, cx
				mov di, offset ds:[P1]
				mov si, offset ds:[P2]
				mov bx, 320
				call calcPQ
				fld qword ptr ds:[qwordBuffer]		
				fstp qword ptr ds:[p]
				mov ax, dx

				mov di, offset ds:[P3]
				mov si, offset ds:[P4]
				mov bx, 200
				call calcPQ
				fld qword ptr ds:[qwordBuffer]		
				fstp qword ptr ds:[q]

				call calcPixel
				cmp al, 1d
				jnb white	
				xor al, al
				jmp showPxl
			white:
				mov al, 0Fh		
			showPxl:
				mov ah, 0Ch
				int 10h
				inc cx
				jmp loopRow			;moving to next pixel in row
			endOfInnerLoop:
				xor cx, cx			;beginning of row
				inc dx				;next row
				jmp loopColumn
		
		showEnd:
			
			popf
			popa
			ret
	showSet endp


	calcPQ proc ;bx - size, di - first arg, si - second, ax - N
			push dx
			fld qword ptr ds:[si]			
			fld qword ptr ds:[di]		 ;st(3) - xMin
			mov dx, bx
			mov word ptr ds:[wordBuffer], dx	
			fild word ptr ds:wordBuffer	
			mov word ptr ds:[wordBuffer], ax			;st(2) - dim
			fild word ptr ds:[wordBuffer]		;st(1) - bx
			fldz							;st(0) - p=0
			
			fadd st(0), st(4)			;B
			fsub st(0), st(3)			;B - A
			fmul st(0), st(1)			;N*(B - A)
			fdiv st(0), st(2)			;N*(B - A)/RES
			fadd st(0), st(3)			;A+ N*(B - A)/RES
			
			fstp qword ptr ds:[qwordBuffer]			;save P and pop
			fsubp st(0), st(0)			
			fsubp st(0), st(0)
			fsubp st(0), st(0)
			fsubp st(0), st(0)
			pop dx
			ret
	calcPQ endp

	calcPixel proc 
			push cx
			fld qword ptr ds:[p]		;st(7) -  P
			fld qword ptr ds:[q]		;st(6) -  Q
			fldz					;st(5) - TMP
			fldz					;st(4) - X
			fldz					;st(3) - X*X
			fldz					;st(2) -  Y
			fldz					;st(1) - Y*Y
			fldz					;st(0) - 
			
			mov cx, 1000d			; 1000 times
			
		innerLoop:
			 ;TMP
			fsub st(0), st(0)		;zeruje rejestr st(0)
			fadd st(0), st(3)		;st(0):=x*x
			fsub st(0), st(1)		;st(0):=x*x-y*y
			fadd st(0), st(7)		;st(0):=x*x-y*y+p
			fxch st(5)				;tmp= st(5):=x*x-y*y+p
			; Y
			fsub st(0), st(0)		;st(0):=0
			fadd st(0), st(4)		;st(0):=x
			fmul st(0), st(2)		;st(0):=x*y
			fadd st(0), st(0)		;st(0):=2*x*y
			fadd st(0), st(6)		;st(0):=2*x*y+q
			fxch st(2)				;y:=2*x*y+q
			; X
			fsub st(0), st(0)		;st(0):=0
			fadd st(0), st(5)		;st(0):=tmp
			fxch st(4)				;x:=tmp
			; x*x + y*y
			fsub st(0), st(0)
			fadd st(0), st(4)		;st(0):=x
			fmul st(0), st(0)		;st(0):=x*x
			fxch st(3)				;st(3):=x*x
			
			fsub st(0), st(0)		;st(0):=0
			fadd st(0), st(2)		;st(0):=y
			fmul st(0), st(0)		;st(0):=y*y
			fxch st(1)				;st(1):=y*y
			
			fsub st(0), st(0)		;st(0):=0
			fadd st(0), st(3)		;st(0):=x*x
			fadd st(0), st(1)		;st(0):=x*x+y*y
			;break?
			fcom ds:[MAX]
			fstsw ax		; FPU do AX
			sahf			;"rejestr flag"
			ja exitOver		
			loop innerLoop		;100o times
			
			mov al, 1d		;petla skonczyla sie sama -> ustawiam flage
			jmp positiveEnd
		exitOver:
			mov al, 0d		;petla zostala przerwana
		
		positiveEnd:
			fsubp st(0), st(0)
			fsubp st(0), st(0)
			fsubp st(0), st(0)
			fsubp st(0), st(0)
			fsubp st(0), st(0)
			fsubp st(0), st(0)
			fsubp st(0), st(0)
			fsubp st(0), st(0)
		
			pop cx
			ret
	calcPixel endp


	parse_arguments proc  ; PSP ->parsed_arguments; and here's copied parser
		push	di
		push	si
		push	es
		push	dx
		push	cx
		push	bx
		push 	ax
		mov	ah, 62h	;  Program Segment Prefix
	    int	21h		; adress in BX
	    mov	es, bx	;  PSP
		mov cl, es:[80h] ;  length of params 
	    cmp cl, 0h
			je lack_of_args  ;errors
		xor cl, cl ;lengh of arg
		mov	si, 82h ;the first sign of params
		mov bx, offset length_of_args	;index of first element
	    mov di, offset parsed_arguments	;index of first element
		call eat_whitespaces
		loop_parse:
			mov	al, es:[si]		; sign
			cmp	al, 13d	
				je EOLOOP;
			cmp al, ' '
				je first_white_space_in_row
			cmp al, 9h  ;tab
				je first_white_space_in_row		
			mov	byte ptr ds:[di], al  ;adding to array
			inc cl 
			jmp not_white_space ;omminting part below if not WS
			first_white_space_in_row:
				mov byte ptr ds:[bx], cl
				inc bx
				xor cl, cl
				mov al, 0  ;end of arg
				mov	byte ptr ds:[di], al
				call eat_whitespaces  
				dec si ;ill do it again below	
			not_white_space:
			inc di
			inc si
			cmp	al, 13d	
			jne	loop_parse ;repeat until ENTER
		EOLOOP:
		;dec cl
		mov byte ptr ds:[bx], cl ;last byte, 13d
		pop 	ax
		pop		bx
		pop		cx
		pop		dx
		pop		es
		pop		si
		pop		di
		ret
	parse_arguments endp		
	
	eat_whitespaces proc	
		dec si
		eat:
		inc si
		mov al, es:[si] ;next sign
		cmp al, ' '
			je eat
		cmp al, 9h  ;tab
			je eat
		ret	

	eat_whitespaces endp

	check_arguments proc ;changing ax and si
		
		mov al, ds:[length_of_args+3]
		cmp al, 0
		jne ok1
		jmp lack_of_args

		ok1:
		mov al, ds:[length_of_args+4]
		cmp al, 0
		je ok2
		jmp too_many_args
		ok2:
		ret

	check_arguments endp

	;ERRORS
	too_many_args:
	mov dx, offset TMA
	jmp end_of_program			
	lack_of_args:
	mov dx, offset LOG
	jmp end_of_program

	wrong_arguments:
	mov dx, offset WA
	jmp end_of_program

	end_of_program:
	mov ah, 9h
	int 21h

	end_of_program_without_exception:
	mov	ah,4ch
	int	21h

code1	ends

stos1	segment stack
	dw	200 dup(?)
top1	dw	?
stos1	ends

end start1	
