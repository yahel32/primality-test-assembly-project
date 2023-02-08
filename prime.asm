IDEAL
MODEL small
STACK 100h
DATASEG
Factors dw 16 dup (0)
digits db 5 dup (0)
first_primes dw 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ;25 primes under 100
seed db 0
a dw 0
b dw 0
p dw 0
emod_r dd 0
prime_text db 'nice! this is a prime!',10,13,'$'
not_prime_text db 'oh.. this is not  a prime. you will get it next time',10,13,'$'
test_num dw 0
is_p db 0              ;1-> prime      0-> not prime


text_main_title db 'MAIN MENU',10,10,10,13,'$' ;text with the main menu title
text_main_start db 'Press S to start',10,10,13,'$' ;text with the start message
text_main_guide db 'Press G to see the guide',10,10,13,'$' ;text with the guide message
text_main_exit db 'Press E to exit',10,10,13,'$' ;text with exit message

guide_title db 'GUIDE',10,10,10,13,'$' ;guide as title
guide_text1 db 'this is a program to test if a number is prime',10,10,13,'$'
guide_text2 db 'you can write any number from 2-65535 and we will test',10,10,13,'$'
guide_text3 db 'if the number is prime. if so we will tell you but',10,10,13,'$'
guide_text4 db 'in case it is not, we will give you the factors',10,10,10,10,13,'$'
guide_exit db 'Press R to return to the main menu',10,10,10,10,13,'$'


test_title db 'TEST',10,10,10,13,'$' ;test as a title
test_text1 db 'write a number between 2-65535, Press enter when you finish.',10,10,13,'$'
test_text2 db 'Press Backspace to delete digits',10,10,13,'$'



prime_title db 'PRIME!',10,10,10,13,'$'
prime_text1 db 'nice!, the number:','$'
prime_text2 db 'is prime',10,10,13,'$'
prime_text3 db 'you can check yourself if you want..',10,10,10,10,13,'$'
prime_text4 db 'Press R to test again any number you want',10,10,13,'$'
prime_text5 db 'Press M to go to the main menu',10,10,13,'$'
prime_text6 db 'Press E to exit',10,10,13,'$'


n_prime_title db 'COMPOSITE',10,10,10,13,'$'
n_prime_text1 db 'oh.. the number:','$'
n_prime_text2 db 'is composite(not prime)',10,10,13,'$'
n_prime_text3 db 'these are the prime factors:',10,13,'$'

n_prime_text4 db 10,10,13,'$'
n_prime_text5 db 'Press R to test again any number you want',10,13,'$'
n_prime_text6 db 'Press M to go to the main menu',10,13,'$'
n_prime_text7 db 'Press E to exit',10,13,'$'


mid db 10,10,10,10,10,10,'$'
digit1 db '-'
digit2 db '-'
digit3 db '-'
digit4 db '-'
digit5 db '-'
midl db '                 ','$'
key db 0


CODESEG


proc tav_input ;wait for charecter
	push ax
	mov ah,1
	int 21h
	mov [key],al
	pop ax
	ret
endp tav_input


proc user_input ;wait for key press
	push ax
	mov ah,0h
	int 16h
	mov [key],al
	pop ax
	ret
endp user_input

proc start_text_mode ;start text mode
	push ax
	xor ah,ah
	mov al,2
	int 10h ;text mode
	pop ax
	ret
endp start_text_mode

proc print_text   ;print text
;assume offset text is in the stack
	push bp
	mov bp,sp
	push ax
	push dx
	mov dx,[bp+4]
	mov ah,9h
	int 21h ;print
	pop dx
	pop ax
	pop bp
	ret 2
endp print_text

proc main_screen
call start_text_mode
	
	push offset mid
	call print_text
	
	push offset midl
	call print_text
	push offset text_main_title
	call print_text
	
	push offset midl
	call print_text
	push offset text_main_start
	call print_text
	
	push offset midl
	call print_text
	push offset text_main_guide
	call print_text
	
	push offset midl
	call print_text
	push offset text_main_exit
	call print_text
	ret

endp main_screen


proc guide_screen
	call start_text_mode
	
	push offset mid
	call print_text
	
	push offset midl
	call print_text
	push offset guide_title
	call print_text
	
	push offset midl
	call print_text
	push offset guide_text1
	call print_text
	
	push offset midl
	call print_text
	push offset guide_text2
	call print_text
	
	push offset midl
	call print_text
	push offset guide_text3
	call print_text
	
	push offset midl
	call print_text
	push offset guide_text4
	call print_text
	
	push offset midl
	call print_text
	push offset guide_exit
	call print_text
	
	ret
endp guide_screen




proc test_screen
	
	
	call start_text_mode
	
	push offset mid
	call print_text
	
	push offset midl
	call print_text
	push offset test_title
	call print_text
	
	push offset midl
	call print_text
	push offset test_text1
	call print_text
	
	push offset midl
	call print_text
	push offset test_text2
	call print_text
	
	push offset midl
	call print_text
	
	mov ah,2
	mov dl,[digit1]
	int 21h
	
	mov dl,[digit2]
	int 21h
	
	mov dl,[digit3]
	int 21h
	
	mov dl,[digit4]
	int 21h
	
	mov dl,[digit5]
	int 21h
	
	
	ret
endp test_screen


proc prime_test ;use Fermat's Litlle Therorem to decide if a given number in range(2-2**16-1) is prime
	;assume the number is in the stack
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	mov ax,[bp+4] ;ax=number
	xor cl,cl
	cmp ax,100
	ja test1
	mov si,offset first_primes
	check_first_prime:
	cmp ax,[si]
	je PRIME
	inc si
	inc si
	inc cl
	cmp cx,25
	jne check_first_prime
	jmp NOT_PRIME
	test1:
	call random_seed;we randomized our seed
	xor cl,cl
	mov bx,ax
	dec bx
	check_prime:
	call random
	push dx
	mov dl,[seed]
	xor dh,dh
	cmp dx,bx;we want seed<=ax-1
	pop dx
	ja check_prime
	mov dl,[seed]
	xor dh,dh
	push dx
	push ax
	push ax
	call emod ;emod_r=seed*n mod n
	mov dx,[word ptr emod_r]
	push bx
	xor bx,bx
	mov bl,[seed]
	cmp dx,bx
	pop bx
	jne NOT_PRIME
	inc cl
	cmp cl,10
	jne check_prime
	jmp prime
	
	
	
	PRIME:
	mov [is_p],1
	jmp prime1
	
	
	NOT_PRIME:
	mov [is_p],0
	
	prime1:
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
	
endp prime_test


proc emod ;compute a**b mod p
	;assume a b and p in the stack
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	
	mov ax,[bp+8]
	mov [a],ax
	mov ax,[bp+6]
	mov [b],ax     
	mov ax,[bp+4]
	mov [p],ax ;a=a b=b p=p
	
	cmp [b],0
	jne cont_
	mov [word ptr emod_r],1
	mov [word ptr emod_r+2],0
	jmp fin
	cont_:
	
	
	mov ax,[b]
	shr ax,1
	jc not_divide_2
	
	divide2:
	push [a]
	push ax
	push [p]
	
	mov ax,[a]
	mov bx,[b]
	call emod ;[emod_r]=emod(a,b/2,p)
	mov [a],ax
	mov [b],bx
	
	mov ax,[word ptr emod_r]
	mul ax
	mov [word ptr emod_r],ax
	mov [word ptr emod_r+2],dx ;we squered emod_r
	
	mod_P:
	mov ax,[p]
	mov bx,[word ptr emod_r]
	cmp bx,ax
	ja undone
	cmp bx,ax
	je undone
	
	cmp [word ptr emod_r+2],0
	je fin
	
	dec [word ptr emod_r+2]
	mov ax,65535
	mov bx,[p]
	dec bx
	sub ax,bx
	add ax,[word ptr emod_r]
	mov [word ptr emod_r],ax
	jmp mod_P
	
	undone:
	sub [word ptr emod_r],ax
	jmp mod_P
	
	
	not_divide_2:
	
	push [a]
	mov ax,[b]
	dec ax
	push ax
	push [p] ;we pushed a,b-1,p
	
	mov ax,[a]
	mov bx,[b]
	call emod;[emod_r]=emod(a,b-1,p)
	mov [a],ax
	mov [b],bx
	
	mov ax,[a]
	mov bx,[word ptr emod_r]
	mul bx
	mov [word ptr emod_r],ax
	mov [word ptr emod_r+2],dx
	
	;now we need to compute emod_r mod p
	
	mod_P1:
	mov ax,[P]
	cmp [word ptr emod_r],ax
	jae undone1
	cmp [word ptr emod_r+2],0
	je fin
	
	dec [word ptr emod_r+2]
	mov ax,65535
	mov bx,[p]
	dec bx
	sub ax,bx
	add ax,[word ptr emod_r]
	mov [word ptr emod_r],ax
	jmp mod_P1
	
	undone1:
	sub [word ptr emod_r],ax
	jmp mod_P1
	
	
	fin:
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 6
endp emod


proc random_seed ;seed=the time(secondes)%30, if (time(secondes)%30)%4==0 then we add 1
	push ax
	push bx
	push cx
	push dx
	mov ah,2ch
	int 21h         ;ch=hours       cl=minuets        dh=secondes   dl= hundredth of a seconed
	mov al,dh
	xor ah,ah
	mov bl,30
	div bl     ;al=ax div 30        ah= ax mod 30
	mov al,ah
	xor ah,ah                   
	mov cl,al
	mov bl,4
	div bl    ;al=ax div 4           ah=ax mod 4
	cmp ah,0
	jne here
	inc cx
	mov [seed],cl
	here:
	mov [seed],cl
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp random_seed

proc random  ;gets a seed and generats a random number and also changes the seed so the next time we use the function we will get something else, range=(0-255)
			; we would probably make the first seed be the first digit of the prime we are testing or the time.
	;assume the value of the seed is in the variable seed
	push ax
	push bx
	push dx
	xor ax,ax
	xor bx,bx
	xor dx,dx
	mov al,[seed]
	mov bl,7
	mul bl
	add ax,32
	xor dx,dx
	mov bx,256
	div bx
	mov [seed],dl
	pop dx
	pop bx
	pop ax
	ret
endp random

proc set_digits
	
	mov [digit1],'-'
	mov [digit2],'-'
	mov [digit3],'-'
	mov [digit4],'-'
	mov [digit5],'-'
	
	ret
endp set_digits

proc divisors_check ;find the divisors of a given number and store them in a given array with length 16 and size dw, the given array is full with zeros
	;assume the number and the offset of the arr we want to check is in the stack [bp+4]=number [bp+6]=arr offset
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	mov si,[bp+6] ;si=offset arr
	mov bx,[bp+4] ; bx=number
	mov ax,[bp+4] ; ax=number
	
	xor cx,cx
	mov cx,2
	xor dx,dx
	L1: ;cl goes through every number up to sqrt(given number) and checks divisibility
		xor dx,dx
		div cx      ;ax= ax/cx   dx=dx:ax mod cx ->dx=0 ,dx=mod -> if dx=0 then the the cx divide the number
		cmp dx,0
		jne check
		mov [si],cx; the dividor is in the arr
		add si,2
		mov bx,ax ; we keep the new number and keep checking
		jmp L1

	check:
	mov ax,bx
	cmp ax,1
	je finish
	xor dx,dx
	inc cx
	jmp L1
	finish:
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
endp divisors_check

proc prime_screen
	call start_text_mode
	
	push offset mid
	call print_text
	
	
	push offset midl
	call print_text
	push offset prime_title
	call print_text
	
	push offset midl
	call print_text
	push offset prime_text1
	call print_text
	
	
	
	mov ah,2
	mov dl,[digit1]
	int 21h
	
	mov dl,[digit2]
	int 21h
	
	mov dl,[digit3]
	int 21h
	
	mov dl,[digit4]
	int 21h
	
	mov dl,[digit5]
	int 21h
	
	
	mov dl,' '
	int 21h

	push offset prime_text2
	call print_text
	
	push offset midl
	call print_text
	push offset prime_text3
	call print_text
	
	push offset midl
	call print_text
	push offset prime_text4
	call print_text
	
	push offset midl
	call print_text
	push offset prime_text5
	call print_text
	
	push offset midl
	call print_text
	push offset prime_text6
	call print_text
	
	ret
endp prime_screen

proc not_prime_screen
	
	
	xor cl,cl
	mov si,offset Factors
	L0_Factor:
	mov [si],0
	inc cl
	add si,2
	cmp cl,16
	jne L0_Factor          ;make factors 00000000000
	
	mov [digits],0
	mov [digits+1],0
	mov [digits+2],0
	mov [digits+3],0
	mov [digits+4],0
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	push offset Factors
	push [test_num]
	call divisors_check ;Factors=factors of the number we already test
	
	call start_text_mode
	
	push offset mid
	call print_text
	
	
	push offset midl
	call print_text
	push offset n_prime_title
	call print_text
	
	push offset midl
	call print_text
	push offset n_prime_text1
	call print_text
	
	
	mov ah,2
	mov dl,[digit1]
	int 21h
	
	mov dl,[digit2]
	int 21h
	
	mov dl,[digit3]
	int 21h
	
	mov dl,[digit4]
	int 21h
	
	mov dl,[digit5]
	int 21h

	
	mov dl,' '
	int 21h
	

	push offset n_prime_text2
	call print_text
	
	push offset midl
	call print_text
	push offset n_prime_text3
	call print_text
	
	push offset midl
	call print_text
	
	
	mov cl,0
	mov si,offset Factors
	L_Factor:
	cmp [si],0
	je done_print_Factor_mid2
	
	
	push ax
	push bx
	push cx
	push dx
	
	L0:
	xor dx,dx
	mov ax,[si]
	mov bx,10
	
	
	cmp ax,10
	jge cont
	xor dx,dx
	div bx   ; ax= ax/10       dx= ax mod 10
	mov [digits+4],dl
	jmp done_find_digit
	cont:
		xor dx,dx
	div bx   ; ax= ax/10       dx= ax mod 10
	mov [digits+4],dl
	
	cmp ax,10
	jge cont1
		xor dx,dx
	div bx   ; ax= ax/100       dx= ax mod 100
	mov [digits+3],dl
	jmp done_find_digit
	cont1:
		xor dx,dx
	div bx   ; ax= ax/100       dx= ax mod 100
	mov [digits+3],dl
	
	cmp ax,10
	jge cont2
		xor dx,dx
	div bx   ; ax= ax/1000       dx= ax mod 1000
	mov [digits+2],dl
	jmp done_find_digit
	cont2:
		xor dx,dx
	div bx   ; ax= ax/1000       dx= ax mod 1000
	mov [digits+2],dl
	
	
	
	cmp ax,10
	jge cont3
		xor dx,dx
	div bx   ; ax= ax/10000       dx= ax mod 10000
	mov [digits+1],dl
	jmp done_find_digit
	
	done_print_Factor_mid2:
	jmp done_print_Factor_mid
	
	
	cont3:
		xor dx,dx
	div bx   ; ax= ax/10000       dx= ax mod 10000
	mov [digits+1],dl
	
	cmp ax,10
	jge cont4
		xor dx,dx
	div bx   ; ax= ax/100000       dx= ax mod 100000
	mov [digits],dl
	jmp done_find_digit
	cont4:
		xor dx,dx
	div bx   ; ax= ax/100000       dx= ax mod 100000
	mov [digits],dl
	
	done_find_digit:
	
	
	cmp [digits],0
	je dig2
	mov dh,[digits]
	add dh,30h
	mov dl,dh
	mov ah,2
	int 21h
	jmp reg_check1
	
	done_print_Factor_mid:
	jmp done_print_Factor
	
	
	dig2:
	cmp [digits+1],0
	je dig3
	
	mov dh,[digits+1]
	add dh,30h
	mov dl,dh
	mov ah,2
	int 21h
	jmp reg_check2
	
	dig3:
	cmp [digits+2],0
	je dig4
	
	mov dh,[digits+2]
	add dh,30h
	mov dl,dh
	mov ah,2
	int 21h
	jmp reg_check3
	
	L_Factor_mid:
	jmp L_Factor
	
	dig4:
	cmp [digits+3],0
	je dig5
	
	mov dh,[digits+3]
	add dh,30h
	mov dl,dh
	mov ah,2
	int 21h
	jmp reg_check4
	
	dig5:
	mov dh,[digits+4]
	add dh,30h
	mov dl,dh
	mov ah,2
	int 21h
	
	jmp fin_dig
	
	L_Factor_mid2:
	jmp L_Factor_mid
	
	reg_check0:
	
	mov dh,[digits]
	add dh,30h
	mov dl,dh
	mov ah,2
	int 21h
	
	reg_check1:
	
	mov dh,[digits+1]
	add dh,30h
	mov dl,dh
	mov ah,2
	int 21h
	
	reg_check2:
	
	mov dh,[digits+2]
	add dh,30h
	mov dl,dh
	mov ah,2
	int 21h
	
	reg_check3:
	
	mov dh,[digits+3]
	add dh,30h
	mov dl,dh
	mov ah,2
	int 21h
	
	reg_check4:
	
	mov dh,[digits+4]
	add dh,30h
	mov dl,dh
	mov ah,2
	int 21h
	
	fin_dig:
	
	mov dl,','
	mov ah,2
	int 21h
	
	mov dl,' '
	mov ah,2
	int 21h
	
	
	pop dx
	pop cx
	pop bx
	pop ax
	
	mov [si],0
	add si,2
	inc cl
	cmp cl,16 
	jne L_Factor_mid2
	
	done_print_Factor:
	
	push offset n_prime_text4
	call print_text
	
	push offset midl
	call print_text
	push offset n_prime_text5
	call print_text
	
	
	push offset midl
	call print_text
	push offset n_prime_text6
	call print_text
	
	push offset midl
	call print_text
	push offset n_prime_text7
	call print_text
	
	
	ret
endp not_prime_screen

proc delete_next_digit
	push ax
	push bx
	push cx
	push dx
	
	mov cl,5
	
	digitL5:
	
	cmp cl,5
	jne d4
	
	cmp [digit5],'-'
	je search1
	mov [digit5],'-'
	jmp done_delete
	
	d4:
	
	cmp cl,4
	jne d3
	
	cmp [digit4],'-'
	je search1
	mov [digit4],'-'
	jmp done_delete
	
	
	
	d3:
	
	cmp cl,3
	jne d2
	
	cmp [digit3],'-'
	je search1
	mov [digit3],'-'
	jmp done_delete
	
	d2:
	
	cmp cl,2
	jne d1
	
	cmp [digit2],'-'
	je search1
	mov [digit2],'-'
	jmp done_delete
	
	d1:
	cmp [digit1],'-'
	je search1
	mov [digit1],'-'
	jmp done_delete
	
	
	search1:
	dec cl
	cmp cl,0
	jne digitL5
	
	done_delete:
	pop dx
	pop cx
	pop bx
	pop ax

	ret
endp delete_next_digit

proc is_digit ;check if a charecter is a digit(0<=number<=9) and ax=1 if digit or 0 if not
	;assume the charecter is in the stack
	push bp
	mov bp,sp ;[bp+4]=charecter
	;save registers
	mov ax,[bp+4]
	cmp ax,30h
	jb not_digit
	cmp ax,39h
	ja not_digit
	mov ax,1
	jmp digit
	not_digit:
	mov ax,0
	digit:
	pop bp
	ret 2
endp is_digit


proc prime_sound
endp prime_sound


proc not_prime_sound
endp not_prime_sound

proc fill_next_digit
	push bp
	mov bp,sp
	
	push ax
	push bx
	push cx
	push dx
	
	
	mov al,[key]
	mov cl,0
	digitL:
	
	cmp cl,0
	jne n1
	cmp [digit1],'-'
	jne search
	mov [digit1],al
	jmp rewrite
	
	
	n1:
	
	cmp cl,1
	jne n2
	cmp [digit2],'-'
	jne search
	mov [digit2],al
	jmp rewrite
	
	
	n2:
	
	cmp cl,2
	jne n3
	cmp [digit3],'-'
	jne search
	mov [digit3],al
	jmp rewrite
	
	
	n3:
	
	cmp cl,3
	jne n4
	cmp [digit4],'-'
	jne search
	mov [digit4],al
	jmp rewrite
	
	
	n4:
	
	cmp [digit5],'-'
	jne search
	mov [digit5],al
	jmp rewrite
	
	
	search:
	inc cl
	cmp cl,5
	jne digitL
	
	
	rewrite:
	
	pop bp
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp fill_next_digit
	


start:
	mov ax,@data
	mov ds,ax
	
	
	;main loop
	start_main_loop:
	
	call main_screen
	
	main_loop:
	
	call user_input
	
	cmp [key],'e'
	je exit_mid
	cmp [key],'E'
	je exit_mid
	
	cmp [key],'g'
	je guide_loop_start
	cmp [key],'G'
	je guide_loop_start
	
	cmp [key],'s'
	je test_loop_start
	cmp [key],'S'
	je test_loop_start
	
	jmp main_loop
	
	
;guide loop
	guide_loop_start:
	
	call guide_screen
	
	guide_loop:
	
	call user_input
	
	cmp [key],'r'
	je start_main_loop
	cmp [key],'R'
	je start_main_loop
	jmp guide_loop
	
	exit_mid:
	jmp exit
	
;stop10
	stop10:
	
	start_main_loop_stop10:
	jmp start_main_loop
;test loop
	test_loop_start:
	
	call test_screen
	
	test_loop:
	call tav_input
	
	xor ax,ax
	mov al,[key]
	push ax
	
	call is_digit
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	cmp ax,1
	jne check_delete_enter
	
	call fill_next_digit
	jmp test_loop_start ;done filling the new digit
	
	
		
		
	check_delete_enter:
	cmp [key],8h     ;if delete was pressed
	jne check_enter
	
	call delete_next_digit ;delete next digit
	jmp test_loop_start_stop8
;stop8
	stop8:
	
	test_loop_Start_stop8:
	jmp test_loop_Start
	
	start_main_loop_stop8:
	jmp start_main_loop_stop10
	
	
	check_enter:
	
	cmp [key],13
	jne test_loop_Start_stop8
	
	xor cx,cx 
	xor bx,bx
	
	mov bl,[digit1]
	push bx
	call is_digit
	add cx,ax
;stop7
	jmp stop7_end
	stop7:
	
	test_loop_Start_stop7:
	jmp test_loop_Start_stop8
	
	start_main_loop_stop7:
	jmp start_main_loop_stop8
	
	stop7_end:
	
	mov bl,[digit2]
	push bx
	call is_digit
	add cx,ax
	
	mov bl,[digit3]
	push bx
	call is_digit
	add cx,ax
	
	mov bl,[digit4]
	push bx
	call is_digit
	add cx,ax
	
	mov bl,[digit5]
	push bx
	call is_digit
	add cx,ax
	
	cmp cx,5
	jne test_loop_Start_stop7
	
	jmp stop6_end
;stop 6
	stop6:
	
	test_loop_Start_stop6:
	jmp test_loop_Start_stop7
	
	start_main_loop_stop6:
	jmp start_main_loop_stop7
	
	stop6_end:
	
	mov bl,[digit1]
	sub bl,30h
	cmp bl,6
	jb test_now
	cmp bl,6
	ja test_loop_Start_stop6
	
	
	mov bl,[digit2]
	sub bl,30h
	cmp bl,5
	jb test_now
	cmp bl,5
	ja test_loop_Start_stop6
	
	
	mov bl,[digit3]
	sub bl,30h
	cmp bl,5
	jb test_now
	cmp bl,5
	ja test_loop_Start_stop6
	
	
	mov bl,[digit4]
	sub bl,30h
	cmp bl,3
	jb test_now
	cmp bl,3
	ja test_loop_Start_stop6
	
	
	mov bl,[digit5]
	sub bl,30h
	cmp bl,5
	jb test_now
	cmp bl,5
	ja test_loop_Start_stop6
	jmp stop5_end
	
;stop5
	stop5:
	test_loop_Start_stop5:
	jmp test_loop_Start_stop6
	
	start_main_loop_stop5:
	jmp start_main_loop_stop6
	
	
	stop5_end:
	
	test_now:
	mov [test_num],0
	
	xor ax,ax
	mov al,[digit5]
	sub al,30h
	add [test_num],ax ;num+=digit5
	
	xor ax,ax
	mov al,[digit4]
	sub al,30h
	mov bl,10
	mul bl ;ax=10*digit4
	add [test_num],ax ;num+=10*digit4
	
	xor ax,ax
	mov al,[digit3]
	sub al,30h
	mov bl,100
	mul bl ;ax=100*digit3
	add [test_num],ax ;num+=100*digit3
	
	jmp solve
;stop4
	stop4:
	
	test_loop_Start_stop4:
	jmp test_loop_Start_stop5
	
	start_main_loop_stop4:
	jmp start_main_loop_stop5
	
	
	solve:
	xor ax,ax
	mov al,[digit2]
	sub al,30h
	mov bl,250
	mul bl ;ax=1000*digit2
	add [test_num],ax ;num+=1000*digit2
	
	xor ax,ax
	mov al,[digit2]
	sub al,30h
	mov bl,250
	mul bl ;ax=1000*digit2
	add [test_num],ax ;num+=1000*digit2
	
	jmp stop3_end
;stop3
	stop3:
	test_loop_Start_stop3:
	jmp test_loop_Start_stop4
	
	start_main_loop_stop3:
	jmp start_main_loop_stop4
	
	stop3_end:
	
	
	xor ax,ax
	mov al,[digit2]
	sub al,30h
	mov bl,250
	mul bl ;ax=1000*digit2
	add [test_num],ax ;num+=1000*digit2
	
	xor ax,ax
	mov al,[digit2]
	sub al,30h
	mov bl,250
	mul bl ;ax=1000*digit2
	add [test_num],ax ;num+=1000*digit2
	
	mov cl,0
	L100:
	inc cl
	xor ax,ax
	mov al,[digit1]
	sub al,30h
	mov bl,100
	mul bl ;ax=10000*digit1
	add [test_num],ax ;num+=10000*digit1
	
	cmp cl,100
	jne L100
	
	jmp stop2_end
;stop2
	stop2:
	test_loop_start_stop2:
	jmp test_loop_Start_stop3
	
	start_main_loop_stop2:
	jmp start_main_loop_stop3
	
	stop2_end:

	;now test_num equal the number we are testing so
	push [test_num]
	call prime_test
	
	
	cmp [is_p],1
	je num_is_prime
	
	num_is_not_prime:
	
	call not_prime_screen
	
	
	np_screen:
	call user_input
	cmp [key],'r'
	je set
	cmp [key],'R'
	je set
	
	cmp [key],'M'
	je set
	cmp [key],'m'
	je set
	
	cmp [key],'e'
	je exit1
	cmp [key],'E'
	je exit1
	
	jmp np_screen
	
	set:
	call set_digits
	
	cmp [key],'r'
	je test_loop_Start_stop2
	cmp [key],'R'
	je test_loop_Start_stop2
	
	cmp [key],'M'
	je start_main_loop_stop2
	cmp [key],'m'
	je start_main_loop_stop2
	
	exit1:
	jmp exit
	
;stop1
	test_loop_start_stop1:
	jmp test_loop_start_stop2
	
	start_main_loop_stop1:
	jmp start_main_loop_stop2
	
	
	num_is_prime:
	call prime_screen
	
	p_screen:
	call user_input
	
	cmp [key],'r'
	je test_again
	cmp [key],'R'
	je test_again
	
	cmp [key],'m'
	je main_again
	cmp [key],'M'
	je main_again
	
	cmp [key],'e'
	je exit
	cmp [key],'E'
	je exit
	
	jmp p_screen
	
	main_again:
	call set_digits
	jmp start_main_loop_stop1
	
	test_again:
	call set_digits
	jmp test_loop_start_stop1


exit:
	mov ax, 4c00h
	int 21h
END start
