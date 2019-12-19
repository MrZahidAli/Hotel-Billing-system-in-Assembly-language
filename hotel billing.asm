INCLUDE Irvine32.inc

.DATA

oPrice   DWORD 169, 149, 99, 89, 69, 69, 10, 5                    ; To store the prices of Oriental...
bill     DWORD ?                                                  ; To store the bill...
bool     DWORD ?                                                  ; To store the result of Check... 

welcome  BYTE "                       "
         BYTE " *** Welcome To Restaurant Transylvania *** ", 0ah, 0dh, 0 ; Welcome note...

id       BYTE " Enter 2 : For Customers ", 0ah, 0dh
	     BYTE " Enter 3 : To Exit ", 0ah, 0dh, 0


options  BYTE "                        ----------------------  ", 0ah, 0dh
         BYTE "                        -----  Customer  ----- ", 0ah, 0dh
         BYTE "                        ---------------------- ", 0ah, 0dh, 0ah, 0dh
         BYTE " Enter 1 : To see our Menu and Prices.", 0ah, 0dh
		 BYTE " Enter 3 : To Place an Order.", 0ah, 0dh
		 BYTE " Enter 5 : To Exit.", 0ah, 0dh , 0

		                                                          ; Price Menu...
pMenu    BYTE "                     ------------------------- ", 0ah, 0dh
         BYTE "                     -- Presenting our Menu -- ", 0ah, 0dh
         BYTE "                     ------------------------- ", 0ah, 0dh, 0ah, 0dh
oriental BYTE " -------------- ", 0ah, 0dh
         BYTE " -- Oriental -- ", 0ah, 0dh
         BYTE " -------------- ", 0ah, 0dh
         BYTE " Enter 1 : Chicken Quorma   : 169 per Dish. ", 0ah, 0dh
         BYTE " Enter 2 : Pullao           : 149 per Dish. ", 0ah, 0dh
	     BYTE " Enter 3 : Chicken Briyani  :  99 per Dish. ", 0ah, 0dh
         BYTE " Enter 4 : Chicken Karahi   :  89 per Dish. ", 0ah, 0dh
         BYTE " Enter 5 : Chicken Tikka    :  69 per Dish. ", 0ah, 0dh
         BYTE " Enter 6 : Murgh Haleem     :  69 per Dish. ", 0ah, 0dh
	     BYTE " Enter 7 : Naan             :  10 per Piece. ", 0ah, 0dh
         BYTE " Enter 8 : Roti             :  05 per Piece. ", 0ah, 0dh
		 BYTE " Enter 9 : To Exit. ", 0ah, 0dh, 0


saleFileName BYTE "Sales.txt", 0		;;need for other

dishes   BYTE " Enter the Quantity:  ", 0

caption  BYTE " Error ", 0
errMsg   BYTE " Please Enter Valid Numbber... ", 0
billMsg  BYTE "    |Total Bill Is:   Rs ", 0
exitMsg  BYTE "   Glad to have you Here... ", 0ah, 0dh

.CODE

main PROC
     mov eax, cyan
	 call setTextColor

     call crlf

	 mov edx, OFFSET welcome                                      ; Printing Welcome...
	 call writeString

     op:
	    call crlf

	    mov edx, OFFSET id                                        ; Printing Id...
		call writeString

		call crlf
		call readInt
		call clrscr

		cmp eax, 1
		
		cmp eax, 2
		je cu

		cmp eax, 3
		je  _exit

		call error                                                ; calling error Proc...
		jmp  op


		    
		cu:
		   call customer
		   jmp op

	 _exit:
		   mov edx, OFFSET exitMsg                                ; Printing Exit Note/Msg...
	       call writeString

	       exit
main ENDP

;-------------------------------------------------------------------
;| For customers only...                                            |
;| Uses: It deals with the customers and take order...              |
;| Note: It only write bill in file with (False) customer name...   |
;-------------------------------------------------------------------

customer PROC
          PUSHAD
		  PUSHFD

	      op:                                                     ; Option Tag...  
		     call crlf

			 mov edx, OFFSET options                              ; Printing options...
	         call writeString

			 call crlf
			 call readInt

			 cmp eax, 1
			 je  pm
			 cmp eax, 3
			 je  cm

			 cmp eax, 5
			 je  _exit

			 call error                                           ; calling error Proc...
			 jmp  op

			 pm:                                                  ; Price Menu Tag...
			    call crlf

		        mov edx, OFFSET pMenu
	            call writeString

			    call crlf
				call waitMsg                                      ; Call Wait Massage...
				call crlf

				jmp  op

			 cm:                                                  ; Choice Menu Tag...				
				call OrientalMenu
				jmp op



    _exit:                                                        ; Exit Tag
		  call printBill
		  mov bill, 0
		  call crlf

		  POPFD
		  POPAD

		  RET
customer ENDP

;-------------------------------------------------------------------
;| Print Oriental Menu with Prices for customers to order...        |
;| Updates: Bill ...                                                |
;-------------------------------------------------------------------

OrientalMenu PROC
			  PUSHAD
			  PUSHFD

			  op:                                                 ; Option Tag...
			     call crlf

		         mov edx, OFFSET oriental
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

		         call error                                       ; calling error Proc...
		         jmp  op

		         cq:                                              ; Chicken Quorma Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

  				    mov ebx, [oPrice]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 pu:                                              ; Pullao Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 4]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 cb:                                              ; Chicken Briyani Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 8]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 ck:                                              ; Chicken Karahi Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 12]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

		         ct:                                              ; Chicken Tikka Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 16]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     mh:                                              ; Murgh Haleem Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 20]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     na:                                              ; Naan Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 24]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     rt:                                              ; Roti Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 28]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

	    _exit:
			  POPFD
			  POPAD

			  RET
OrientalMenu ENDP




;-------------------------------------------------------------------
;| Uses: Print the bill for Customers...                            |
;-------------------------------------------------------------------

printBill PROC
           PUSHAD
		   PUSHFD

		   call crlf

		   mov edx, OFFSET billMsg
	       call writeString 

		   mov eax, bill
		   call writeInt                                         ; Print the original bill...

		   

		   call crlf

		   call crlf
		   call crlf

		   mov edx, OFFSET exitMsg                                 ; Printing Exit Note/Msg...
	       call writeString

		   POPFD
		   POPAD


	       RET
printBill ENDP

;-------------------------------------------------------------------
;| Shows an Error Box to customers...                               |
;| Uses:  2 strings for an input   box...                           |
;| Advan: It also works as a pause...                               |
;-------------------------------------------------------------------

error PROC
       PUSHAD
	   PUSHFD

	   call crlf

       mov ebx, OFFSET caption
	   mov edx, OFFSET errMsg
	   call msgBox

	   POPFD
	   POPAD

	   RET
error ENDP

END main
