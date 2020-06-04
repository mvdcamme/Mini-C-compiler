SECTION .data       
    hola:   db "Hola!",10   
    tam:    equ $-hola      

SECTION .text       
    global main     

main:               

    mov edx,tam     
    mov ecx,hola        
    mov ebx,1       
    mov eax,4       
    int 0x80        

    mov ebx,0       
    mov eax,1       
    int 0x80   
