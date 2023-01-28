IDEAL
MODEL medium
STACK 100h
DATASEG
Factors dw 16 dup (0)
seed db 0
a dw 0
b dw 0
p dw 0
emod_r dd 0
CODESEG
proc user_input
endp user_input


proc prime_test ;use Fermat's Litlle Therorem to decide if a given number in range(2-2**16-1) is prime
endp prime_test


proc emod ;compute a**b mod p
	;assume a b and p in the stack
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	
	mov ax,[bp+10]
	mov [a],ax
	mov ax,[bp+8]
	mov [b],ax     
	mov ax,[bp+6]
	mov [p],ax ;a=a b=b p=p
	
	cmp [b],0
	jne cont
	mov [word ptr emod_r],1
	mov [word ptr emod_r+2],0
	jmp fin
	cont:
	
	
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
	mov cl,2
	xor dx,dx
	L1: ;cl goes through every number up to sqrt(given number) and checks divisibility
		
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
	inc cl
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

proc check_pointer
endp check_pointer

proc show_guide
endp show_guide

proc prime_screen
endp prime_screen

proc not_prime_screen
endp not_prime_screen


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



start:
	mov ax,@data
	mov ds,ax
	
	
	push 251
	push 7369
	push 7369
	call emod
	


exit:
	mov ax, 4c00h
	int 21h
END start

