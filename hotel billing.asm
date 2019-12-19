INCLUDE Irvine32.inc

.DATA

INPUT_SIZE    = 17                                                ; Max User can give as Input...
SALE_SIZE     = 20                                                ; Max Sale Digits to be written to file...
oPrice   DWORD 169, 149, 99, 89, 69, 69, 10, 5                    ; To store the prices of Oriental...
cPrice   DWORD 169, 149, 99, 79                                   ; To store the prices of Chinese...
fPrice   DWORD 149, 99, 79, 49                                    ; To store the prices of Fast Food...
dePrice  DWORD 799, 699, 99, 69                                   ; To store the prices of Dessert...
drPrice  DWORD 99, 99, 49 ,49, 69, 64, 89, 49                     ; To store the prices of Drinks...
bill     DWORD ?                                                  ; To store the bill...
bool     DWORD ?                                                  ; To store the result of Check... 
byteRead DWORD ?                                                  ; To store read Bytes from File...
fHandle  DWORD ?                                                  ; To store File Handle...
mockBill DWORD ?                                                  ; To store copy of Bill...
dealRep  DWORD ?                                                  ; To store Deal Repetition...
bytWrite DWORD ?
userPass BYTE  INPUT_SIZE DUP(?)                                  ; To store the Input Password...
mockSaleBill BYTE ?

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
exitMsg  BYTE "    ~~~~~~~~~~~~~~~~~~~~~~~~~ ", 0ah, 0dh
         BYTE "   |Glad to have you Here... |", 0ah, 0dh
		 BYTE "    ~~~~~~~~~~~~~~~~~~~~~~~~~ ", 0ah, 0dh, 0

newLine BYTE 0ah, 0dh

.CODE
inputPass        PROTO, passString :PTR BYTE					  ; To print Oriental Menu on deals
dealOrientalMenu PROTO, noOfDishes :DWORD						  ; To print Oriental Menu on deals
dealChineseMenu  PROTO, noOfDishes :DWORD						  ; To print Chinese Menu on deals
dealFastFoodMenu PROTO, noOfDishes :DWORD						  ; To print Fast Food Menu on deals
setEcx2          PROTO, dealQuan1  :DWORD					      ; To set the value of ecx
setEcx3          PROTO, dealQuan2  :DWORD						  ; To set the value of ecx
dealDrinks1_5    PROTO , noOfDrinks:DWORD						  ; To 1.5 liters Drink Menu on deals

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
;| Read sales from Sales File...                                    |
;| Uses: saleFile string to store sales...                          |
;| booL = 1 (operation succeeded) && bool = 0 (operation failed)    |
;-------------------------------------------------------------------


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
;| Print Deals and Offers with Prices for customers...              |
;| Uses:   deals string to print...                                 |
;| update: bill according to selected Deals and Offers..            |
;-------------------------------------------------------------------



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
;|          |
;| Uses: Print the bill for Customers...                            |
;-------------------------------------------------------------------
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

;-------------------------------------------------------------------
;| Print Deal Oriental Menu with Prices for customers to order...   |
;| Updates: Bill ...                                                |
;-------------------------------------------------------------------
;-------------------------------------------------------------------
;| Checking number of dishes for deals 2-4                          |
;| Updates: It exit the Loop...                                     |
;-------------------------------------------------------------------

setEcx2 PROC uses eax, dealQuan1:DWORD
	    
		mov eax, dealQuan1
		cmp eax, 2
		jge setECX
				   
		jmp ignore										  ; Check the quantity of dishes to decide the value of ecx register
				   
		setECX:
		mov ecx,1

		ignore:
		RET
setEcx2 ENDP

;-------------------------------------------------------------------
;| Checking number of dishes for deal 1                             |
;| Updates: Updates: It exit the Loop...                            |
;-------------------------------------------------------------------

setEcx3 PROC uses eax, dealQuan2:DWORD
	    
		mov eax, dealQuan2
		cmp eax, 3
		jge setECX
				   
		jmp ignore										  ; Check the quantity of dishes to decide the value of ecx register
				   
		setECX:
		mov ecx,1

		ignore:
		RET
setEcx3 ENDP

END main
