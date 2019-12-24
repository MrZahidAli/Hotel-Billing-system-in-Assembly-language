INCLUDE Irvine32.inc

.DATA

filename BYTE "menueItems.txt",0
filename2 BYTE "price.txt", 0
filename3 BYTE "menue.txt", 0
fileHandler dword ?
msg1 dword 13 dup(?)




PRICES   dword 70,120,90,100,60,50,10,30                 
bill     dword ?                                                 
bool     dword ?                                                  
Start  BYTE "                              "
         BYTE "*************************************",0ah,0dh
		 BYTE "                              "
         BYTE "* !!!  Deans Hotel Food Billing !!! *", 0ah, 0dh 
		 BYTE "                              "
		 BYTE "*************************************",0ah,0dh,0

entering       BYTE " Enter '1':   ", 0ah, 0dh
         BYTE "       For Menu and Billing ", 0ah, 0dh
	     BYTE " Enter '2':  ", 0ah, 0dh
		 BYTE "       To Exit", 0ah, 0dh,0


options  BYTE "                                                 <***************> ", 0ah, 0dh, 0ah, 0dh
         BYTE " Press <1> -> To show available food items.", 0ah, 0dh
		 BYTE " Press <2> -> For Order.", 0ah, 0dh
		 BYTE " Press <3> -> To Exit.", 0ah, 0dh , 0

		                                                         
Selection BYTE 5000 dup(?)
Selection2 BYTE 5000 dup(?)
saleFileName BYTE "Sales.txt", 0		;;need for other

Quantity   BYTE " Enter the Quantity for that item :  ", 0

caption  BYTE " Invalid Input ", 0
errMsg   BYTE " Please Enter Valid One... ", 0
TotalBill  BYTE "     Total Bill for selected items is:   Rs ", 0
Final  BYTE "   Thanks for coming ", 0ah, 0dh

.CODE

main PROC

	mov edx, offset filename	;mov offset of file name wna read
	call OpenInputFile		;open file
	mov edx, offset selection	;to save data from file into msg1
	mov ecx, 1000	;size of data wana read
	Call ReadFromFile	;reading from file

	mov edx, offset filename3	;mov offset of file name wna read
	call OpenInputFile		;open file
	mov edx, offset selection2	;to save data from file into msg1
	mov ecx, 1000	;size of data wana read
	Call ReadFromFile	;reading from file


	 mov edx, OFFSET Start                                    
	 call writeString

     op:
	    call crlf

	    mov edx, OFFSET entering                                       
		call writeString

		call crlf
		call readInt
		call clrscr

		cmp eax, 1
		je cu

		cmp eax, 2
		je  _exit

		call errorHandling                                                
		jmp  op


		    
		cu:
		   call User
		   jmp op

	 _exit:
		   mov edx, OFFSET Final                                
	       call writeString

	       exit
main ENDP

;For user to Enter the billing environment or exit
User PROC
          PUSHAD
		  PUSHFD

	      op:                                                      
		     call crlf

			 mov edx, OFFSET options                            
	         call writeString

			 call crlf
			 call readInt

			 cmp eax, 1
			 je  pm
			 cmp eax, 2
			 je  cm

			 cmp eax, 3
			 je  _exit

			 call errorHandling                                            
			 jmp  op

			 pm:                                                  
			    call crlf

		        mov edx, OFFSET Selection2
	            call writeString

				jmp  op

			 cm:                                                  				
				call MenuItems
				jmp op



    _exit:                                                        
		  call finalBill
		  mov bill, 0
		  call crlf

		  POPFD
		  POPAD

		  RET
User ENDP

;To show Food Items alomng with prices for user
; to Select the items of h.
Menuitems PROC
			  PUSHAD
			  PUSHFD

			  op:                                                 
			     call crlf

		         mov edx, OFFSET Selection
	             call writeString

		         call crlf
		         call readInt

	             cmp eax, 1
		         je  cq
		         cmp eax, 2
		         je  pu
		         cmp eax, 3
		         je  cb
		         cmp eax, 4
		         je  ck 
		         cmp eax, 5
		         je  ct 
				 cmp eax, 6
				 je  mh
				 cmp eax, 7
				 je  na
				 cmp eax, 8
				 je  rt
				 cmp eax, 9
				 je  _exit

		         call errorHandling                                        
		         jmp  op

		         cq:                                              
		            mov edx, OFFSET Quantity
	                call writeString

				    call readInt                                 
  				    mov ebx, [PRICES]
				    mul ebx                                      
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 pu:                                             
	                mov edx, OFFSET Quantity
	                call writeString

				    call readInt                                 

				    mov ebx, [PRICES + 4]
				    mul ebx                                       
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 cb:                                              
	                mov edx, OFFSET Quantity
	                call writeString

				    call readInt                                 

				    mov ebx, [PRICES + 8]
				    mul ebx                                      
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 ck:                                              
		            mov edx, OFFSET Quantity
	                call writeString

				    call readInt                                  ; Taking input for Quantity of a Specific item

				    mov ebx, [PRICES + 12]
				    mul ebx                                       
				    add eax, bill
				    mov bill, eax

		            jmp  op

		         ct:                                              
	                mov edx, OFFSET Quantity
	                call writeString

				    call readInt                                

				    mov ebx, [PRICES + 16]
				    mul ebx                                      
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     mh:                                             
	                mov edx, OFFSET Quantity
	                call writeString

				    call readInt                                  

				    mov ebx, [PRICES + 20]
				    mul ebx                                       
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     na:                                             
	                mov edx, OFFSET Quantity
	                call writeString

				    call readInt                                

				    mov ebx, [PRICES + 24]
				    mul ebx                                     
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     rt:                                             
	                mov edx, OFFSET Quantity
	                call writeString

				    call readInt                                  

				    mov ebx, [PRICES + 28]
				    mul ebx                                     
				    add eax, bill
				    mov bill, eax

		            jmp  op

	    _exit:
			  POPFD
			  POPAD

			  RET
MenuItems ENDP




;Print Final Bill of User

finalBill PROC
           PUSHAD
		   PUSHFD

		   call crlf

		   mov edx, OFFSET TotalBill
	       call writeString 

		   mov eax, bill
		   call writeInt                                        

		   

		   call crlf

		   call crlf
		   call crlf

		   mov edx, OFFSET Final                                 
	       call writeString

		   POPFD
		   POPAD


	       RET
finalBill ENDP

;Procedure for Error Handling

errorHandling PROC
       PUSHAD
	   PUSHFD

	   call crlf

       mov ebx, OFFSET caption
	   mov edx, OFFSET errMsg
	   call msgBox

	   POPFD
	   POPAD

	   RET
errorHandling  ENDP

END main
