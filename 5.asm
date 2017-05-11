
dane1 	segment
handler	dw	?
mode db '2'  ;1 - check, 2 - save; 
buf	db	105 dup('$')
buffer db ?

parsed_arguments		db 127 dup(?)
length_of_args			db 50 dup(0)
LOG	db 	"Lack of arguments$"
FNO db "File not opened$"
TMA db "Too many arguments$"
SO db "Signs OK$"
BS1 db "Bad sign: $"
BS2 db " in line: $"

interSignsCounter dw 0
ZI db "znaki interpuncyjne: $"  ;21
sentenceCounter dw 0
LZ db 13, 10, "liczba zdan: $" ;15
LLN db 13, 10, "liczba linii: $" ;16
letterCounter dw 0
LLT db 13, 10, "liczba liter: $" ;16
numberCounter dw 0
LC db 13, 10, "liczba cyfr: $" ;15
wsCounter dw 0
LBZ db 13, 10, "liczba bialych znakow: $"  ;24
wordCounter dw 0
LW db 13, 10, "liczba wyrazow: $"  ;18

NOLICS db 0 ;number of letter in current sentence
NOLICW db 0 ;number of letter in current word

posS dw 0

lineposition dw 1

dane1	ends
code1	segment

start1:	
	mov	ax,seg top1
	mov	ss,ax
	mov	sp,offset top1
	mov	ax,seg dane1
	mov	ds,ax
	call parse_arguments
	mov si, offset parsed_arguments
	call check_arguments ;and find out version of program
	mov	dx, si
	xor	al,al  ; al=0 -> read-only
	mov	ah,3dh  ;opening file arg
	int	21h ;open
	mov	ds:[handler],ax ;file handler in 'handler'
	jc fine_not_opened
	xor cx, cx
	
	

	CHECK:   ;in this 'loop' i save some bytes to buffor, then analyse them until nothing rests
	mov	dx, offset buf ; 
	mov	bx,ds:[handler]
	push cx
	mov	cx,30d  ;30 bytem crunches
	mov	ah,3fh ;and opening
	int	21h
	pop cx
	mov cl, al 
	cmp cl, 0
	je check_end ;until end
	mov si, offset buf
	check_loop:
		mov dx, ds:[posS]
		inc dx
		mov word ptr ds:[posS], dx
		mov dl, ds:[si]
		cmp dl, 'a'
		jl notsmall
		cmp dl, 'z'
		jg notsmall

		mov bx, ds:[letterCounter];
		inc bx
		mov word ptr ds:[letterCounter], bx
		inc ch
		jmp fine
		notsmall:

		cmp dl, 'A'
		jl notbig
		cmp dl, 'Z'
		jg notbig
		mov bx, ds:[letterCounter];
		inc bx
		mov word ptr ds:[letterCounter], bx
		inc ch
		jmp fine

		notbig:
		cmp dl, '0'
		jl notint
		cmp dl, '9'
		jg notint
		mov bx, ds:[numberCounter];
		inc bx
		mov word ptr ds:[numberCounter], bx
		inc ch
		jmp fine

		notint:
		cmp dl, 10d
		je  new_line
		cmp dl, 13d
		je ommit4
		cmp dl, '?'
		je new_sentenence
		cmp dl, '!'
		je new_sentenence
		cmp dl, '.'
		je new_sentenence
		cmp dl, ':'
		je interpunction
		cmp dl, ' '
		je whitespace
		cmp dl, ';'
		je interpunction
		cmp dl, ','
		je interpunction
		cmp dl, '-'
		je interpunction
		cmp dl, '	'
		je whitespace

			mov bl, ds:[mode] ;here we see it's unexpected sign
			cmp bl, '2'
			je fine
			mov dx, offset BS1
			mov ah, 9
			int 21h 
			mov dx, ds:[posS] 
			call print_registry
			mov dx, offset BS2
			mov ah, 9
			int 21h 
			mov dx, ds:[lineposition]
			call print_registry
			jmp end_of_program_without_exception

		new_line:
		mov bx, ds:[lineposition]
		inc bx
		mov word ptr ds:[lineposition], bx
		
		mov word ptr ds:[posS], 0d
		mov word ptr ds:[NOLICW], 0d
		
		jmp ommit4

		whitespace:
		mov bx, ds:[wsCounter];
		inc bx
		mov word ptr ds:[wsCounter], bx
		mov word ptr ds:[NOLICW], 0d
		jmp ommit4



		new_sentenence:
		mov word ptr ds:[NOLICS], 0d
		;jmp new_line2

		interpunction:
		mov bx, ds:[interSignsCounter];
		inc bx
		mov word ptr ds:[interSignsCounter], bx
		mov word ptr ds:[NOLICW], 0d
		jmp ommit4
		
		

		fine: ;fine means normal signs which may construct words thus senteces - letters and numbers
		
		mov bh, ds:[NOLICS];
		cmp bh, 0
		jne not_first1
			mov ax, ds:[sentenceCounter];
			inc ax
			mov word ptr ds:[sentenceCounter], ax
		
		not_first1:
		inc bh
		mov byte ptr ds:[NOLICS], bh
		mov bh, ds:[NOLICW];
		cmp bh, 0
		jne not_first2
			mov ax, ds:[wordCounter];
			inc ax
			mov word ptr ds:[wordCounter], ax
		not_first2:
		inc bh
		mov byte ptr ds:[NOLICW], bh
		
		ommit4: ;if it's neither letter nor number
		
		
		inc si
		dec cl
		jne check_loop
		jmp CHECK

		check_end:
		mov	bx,ds:[handler]
		mov	ah,3eh
		int	21h
		
		mov bl, ds:[mode]  ; ok, so if we need to write to file, let'sdo it
		cmp bl, '2'
		je write_to_file
		mov dx, offset SO  ; if not, exit please
		jmp end_of_program

		write_to_file:
		  mov  cx, 0
		  mov  dx, offset parsed_arguments
		  inc dx
		  xor ax, ax
		  mov al, ds:[length_of_args]
		  add dx, ax ;second arg
		  mov  ah, 3ch
		  int  21h  
		  mov  word ptr ds:[handler], ax ;PRESERVE FILE HANDLER RETURNED.
		  xor dx,  dx

		  mov  ah, 40h
		  mov  bx, ds:[handler]
		  mov  cx, 21d  ;STRING LENGTH.
		  mov  dx, offset ZI
		  int  21h

		  mov dx, word ptr ds:[interSignsCounter]
		  call save_registry
		  mov  ah, 40h
		  mov  bx, ds:[handler]
		  mov  cx, 15d  ;STRING LENGTH.
		  mov  dx, offset LZ
		  int  21h

		  mov dx, word  ptr ds:[sentenceCounter]
		  call save_registry
		  mov  ah, 40h
		  mov  bx, ds:[handler]
		  mov  cx, 16d  ;STRING LENGTH.
		  mov  dx, offset LLT
		  int  21h

		  mov dx, word  ptr ds:[letterCounter]
		  call save_registry
		  mov  ah, 40h
		  mov  bx, ds:[handler]
		  mov  cx, 16d  ;STRING LENGTH.
		  mov  dx, offset LLN
		  int  21h

		  mov dx, word  ptr ds:[lineposition]
		  call save_registry
		  mov  ah, 40h
		  mov  bx, ds:[handler]
		  mov  cx, 25  ;STRING LENGTH.
		  mov  dx, offset LBZ
		  int  21h

		  mov dx, word  ptr ds:[wsCounter]
		  call save_registry
		  mov  ah, 40h
		  mov  bx, ds:[handler]
		  mov  cx, 15  ;STRING LENGTH.
		  mov  dx, offset LC
		  int  21h

		  mov dx, word  ptr ds:[numberCounter]
		  call save_registry
		  mov  ah, 40h
		  mov  bx, ds:[handler]
		  mov  cx, 18  ;STRING LENGTH.
		  mov  dx, offset LW
		  int  21h

		  mov dx, word  ptr ds:[wordCounter]
		  call save_registry

		  mov  ah, 3eh ;close file 
		  mov  bx, ds:[handler]
		  int  21h  
		jmp end_of_program_without_exception


	print_registry proc ;from_dx 
		push ax
		push dx
		push cx
		push bx
		mov ax, dx
		mov bx, 10d
		xor cx, cx

		more:
		xor dx, dx
		div bx ; ax - result, dx - rest
		push dx
		inc cx
		cmp ax, 0d
		jne more
		mov ah, 2

		more_write:
		pop dx
		add dl, 48d
		int 21h
		loop more_write

		pop bx
		pop cx
		pop dx
		pop ax
		ret
	print_registry endp

	save_registry proc ;from_dx just save number  
		push ax
		push dx
		push cx
		push bx

		mov ax, dx
		mov bx, 10d
		xor cx, cx

		more2:
		xor dx, dx
		div bx ; ax - result, dx - rest
		push dx
		inc cx
		cmp ax, 0d
		jne more2
		mov ah, 2

		more_write2:
		pop dx
		add dl, 48d
		mov byte ptr ds:[buffer], dl
		push cx
		mov  ah, 40h
		  mov  bx, ds:[handler]
		  mov  cx, 1  ;STRING LENGTH.
		  mov  dx, offset buffer
		  int  21h
		pop cx
		loop more_write2

		pop bx
		pop cx
		pop dx
		pop ax
		ret

	save_registry endp

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

	show_arguments proc
		push si
		push dx
		push ax
		mov si, offset parsed_arguments	 ;index of array
		print_loop:
			mov dl, ds:[si] ;actual character ready to ready to be printed
		
			cmp dl, 13d
				je end_of_loop ;stop if ENTER
			mov ah, 2h
			int 21h;	
			inc si
			jmp print_loop
		end_of_loop:
		pop ax
		pop dx
		pop si
		ret
	show_arguments endp

	check_arguments proc ;changing ax and si
		
		mov al, ds:[length_of_args+1]
		cmp al, 0
		jne ok1
		jmp lack_of_args

		ok1:
		mov al, ds:[length_of_args+2]
		cmp al, 0
		je ok2
		jmp too_many_args

		ok2:
		mov al,byte ptr ds:[si]			; first sign in al, second in ah
		mov ah,byte ptr ds:[si+1]		; check if '-v' in ax
		cmp ax,30253d
		jne ommit1
			mov di, offset mode

			mov ah, '1'
			mov ds:[di], ah
			inc si;
			inc si;
			inc si
		ommit1:
		ret

	check_arguments endp

	;ERRORS
	too_many_args:
	mov dx, offset TMA
	jmp end_of_program			
	lack_of_args:
	mov dx, offset LOG
	jmp end_of_program
	fine_not_opened:
	mov dx, offset FNO
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
