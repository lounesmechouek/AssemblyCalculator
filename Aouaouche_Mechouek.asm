   
                                  ;TP 02 : ASSEMBLEUR
;==============================================================================================
               
;Theme: Realisation d'une calculatrice

;On veut realiser une calculatrice de gestion, disposant des 4 operations : 
;    +, -, / et * sans tenir compte de la priorite des operations  

;==============================================================================================
;REALISE PAR :                                             Groupe:02/1CPI A
;        *AOUAOUCHE Louiza
;        *MECHOUEK Lounes 

;ENCADRES PAR:
;        *Mr DAHAMNI
;==============================================================================================

	
s_pile          segment  stack
                dw  60  dup(?)
s_pile          ends

s_donne         segment
    ;INTERVALLE A RESPECTER
    sup          db   03D,02D,07D,06D,07D 
    inf          db   03D,02D,07D,06D,08D 
    
    ;MESSAGES D'AFFICHAGE   
 graphis db 10,13,"    _______________________________________________________________ "
        db 10,13, "   |                                                               | "
        db 10,13, "   |                                                               | "
        db 10,13, "   |                ECOLE nationale Superieure d'Informatique      | "
        db 10,13, "   |                        TP2 EN ASSEMBLEUR                      |"
        db 10,13, "   |                REALISATION D'UNE CALCULATRICE                 | "
        db 10,13, "   |                                                               | "
        db 10,13, "   |       |                             \   /            -        | " 
        db 10,13, "   |       |                              \ /                      | "
        db 10,13, "   |    ---|---        -------             \           -------     | "
        db 10,13, "   |       |                              / \                      | " 
        db 10,13, "   |       |                             /   \            -        | "
        db 10,13, "   |            _                                                  |"
        db 10,13, "   |           |_|                                                 | "
        db 10,13, "   |           /_/     AOUAOUCHE LOUIZA et MECHOUEK LOUNES         |"
        db 10,13, "   |   ___  ___ _         1CPI - GROUPE 02 - 2017/2018             | "     
        db 10,13, "   |  / _ \/ __| |          ENCADREUR : Mr F.Dahamni.               | "       
        db 10,13, "   | |  __/\__ \ |                                                 | "       
        db 10,13, "   |  \___||___/_|                                                 | "          
        db 10,13, "   |                                                               | "
        db 10,13, "   |_______________________________________________________________|",10,13,10,'$'
   
    accueil      db   0Dh,0Ah,"_______________________________________________________________________________",09,09,0Dh,0Ah,"TRAVAIL DEMANDE :",0Dh,0Ah,"   1.	Utilisation des procedures de lecture ecriture realisees dans le TP1 ;",0Dh,0Ah,"   2.	Evaluer l'expression en virgule fixe ;",0Dh,0Ah,"   3.	Afficher tous les resultats intermediaires et final de l'expression",0Dh,0Ah," sur ecran en decimal. ",0Dh,0Ah,"_______________________________________________________________________________",0Dh,0Ah,0Ah,09,09,"VEUILLEZ INTRODUIRE UNE EXPRESSION ARITHMETIQUE:",0Dh,0Ah,09,09,"(Operateurs possibles : + , - , / , * , = )",0Dh,0Ah,"$" 
    operande     db   0Dh,0Ah,"  OPERANDE:                      $"  
    erreur       db   0Dh,0Ah,"NOMBRE ERRONE! VEUILLEZ REESSAYER:$"  
    oper         db   0Dh,0Ah,"  OPERATEUR:                     $"  
    erreur2       db   0Dh,0Ah,"OPERATEUR ERRONE! VEUILLEZ REESSAYER:$"                                                                                    
    erreurcapacite db 0Dh,0Ah,"OPERATION DE MULTIPLICATION IMPOSSIBLE! ESPACE INSUFFISANT.$" 
    div_error      db 0Dh,0Ah,"OPERATION DE DIVISION IMPOSSIBLE! ESPACE INSUFFISANT.$" 
    sous_err     db 0Dh,0Ah,"OPERATION DE SOUSTRACTION IMPOSSIBLE! ESPACE INSUFFISANT.$"
    addit_err      db 0Dh,0Ah,"OPERATION D'ADDITION IMPOSSIBLE! ESPACE INSUFFISANT.$" 
    div_zero       db 0Dh,0Ah,"DIVISION PAR ZERO IMPOSSIBLE. VEUILLEZ REESSAYER.$"
    resul          db 0Dh,0Ah,0Dh,0Ah,"LE RESULTAT DE L'EXPRESSION EST : $"
    ;VARIABLES   
    
    veriferr     db   00h ;booleen qui verifie si nous avons genere une erreur
    dixmille     dw   10000
    arret        db   00h
    max          dw   0000h 
    nombre       dd   00000000h
    operation    db   00h
    cpt          db   00h 
    ind          dw   0000D 
    signe        db   00h 
    dix          dw   0Ah
    virgule      db   00h
    
    ;ZONE DE LECTURE   
    ipretour         dw 0000h
    vect             db 10 DUP ('$')
    tab_ascii        db 10 DUP ('$')
    zone2            dw 02D,00D
    nombre1          dd   00000000h
    nombre2          dd   00000000h
    zone             dw 07D,00D
    zone_sup         dw 10D,00D     
        
    
   
s_donne         ends  

s_code          segment
    
                assume SS:s_pile,DS:s_donne,CS:s_code
                  
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²² 
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²                      ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²² PROCEDURES UTILISEES ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²                      ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²² 
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
 
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
;PROCEDURE D'ARRET
arreter         proc    near
                mov     ah,4CH
                int     21H
arreter         endp 

;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
;;Rend 0 si le caractere est un entier entre 0 et 9, 1 sinon
verifchf        proc   near
                mov    ah,0         ;booleen initialise avrai
                
                cmp    al,'0'       ;on compare avec le code ascii de "0"
                jl    finverifchf   ;si c'est inferieur, on vers le changement du booleen a faux
                cmp    al,'9'       ;on compare avec le code ascii de "9"
                ja    finverifchf   ;si c'est superieur, on vers le changement du booleen a faux
                jmp   FIN           ;si le caractere est compris entre 0 et 9 (inclus) le booleen reste a vrai   
                 
finverifchf:    mov    ah,1 
  FIN:          ret     
verifchf        endp
 
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° PROCEDURE DE LECTURE TP1 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

  
;##############################################################################################
;REMARQUE: si l'utilisateur introduit des espaces le cas est considere comme etant une erreur #
;          car son code ascii n'est ni un signe ni un chiffre, exactement comme scanf en      #
;          langage C.                                                                         #
;##############################################################################################

lirenb          proc near 
                mov ah,9
                lea dx,operande
                int 21h
                  
                mov bx,0000h 
                
        lire:   mov ah,10            ;
                lea dx,zone          ;Fonction de lecture vers une zone memoire
                int 21h              ;
                
;===========================CONTROLES D'ERREURS==============================================

                mov cx,0000h         ;initialisation de cx qui pouvait contenir n'importe quelle valeur
                mov di,1             ;initialisation de l'offset de la zone de lecture
                mov cl,b.zone[di]    ;initialisation du compteur de boucle au nombre d'elements
                mov bl,cl            ;rangement du compteur de boucles pour la conversion(sans passer par une case memoire qui ralentit le programme)
                inc di               ;incrementation pour acceder a l'octet ou a commence la saisie 
                
;--------------> verification du signe --------------------------------------------------------
      
                mov    dl,00h       ;non signe(par defaut) 
                cmp    b.zone[di],2Dh ;si le signe est negatif
                je     signneg      ;on va vers l'enregistrement du signe negatif
                cmp    b.zone[di],2Bh ;si le signe est positif
                je     sigpos       ;on va vers l'enregistrement du signe positif
                jmp    finverifsign ;si aucun signe n'a ete saisi on sort avec dl=00h (nombre non signe)
                
signneg:        mov    dl,2Dh       ;code ascii de "-"
                jmp    finverifsign
                 
sigpos:         mov    dl,2Bh       ;code ascii de "+" 

finverifsign:   

                mov signe,dl         ;rangement du signe
                
               
;TRAITEMENT DES CARACTERES SELON LE SIGNE SAISI
               
                cmp dl,2Bh           ;resultat de la procedure:signe positif saisi
                je CHIFFRE           ;on va vers le traitement des caracteres autres que le signe
                
                cmp dl,2Dh           ;resultat de la procedure:signe negatif saisi
                je CHIFFRE           ;on va vers le traitement des caracteres autres que le signe
                
                jmp passigne         ;si aucun signe n'a ete saisi alors il est non signe
                                                                                            
                                                                                            
;---------------> verification si les chiffres saisis ne sont pas des caracteres spaciaux ou alphabetiques   
       
       CHIFFRE: dec cx               ;on diminiue d'une iteration (car on a traite l'octet du signe apart)
                inc di               ;on fait avancer la position 
                mov bl,cl            ;on sauvegarde le compteur pour l'utiliser ulterierement dans la conversion
                mov ind,di           ;on sauvegarde la position du 1er caractere different du signe pour une eventuelle utilisation
                
   ;Dans le cas ou le signe n'a pas ete saisi,
   ;on n'incremente pas le compteur car le 1er octet correspond a un chiffre
           
       passigne:mov ind,di           ;on sauvegarde la position du 1er caractere different du signe(mais sans l'incrementation)
       
                      
        boucle: mov al,b.zone[di]    ;on affecte un caractere a al pour le traiter a part
                call verifchf        ;on appelle la procedure de verification de caracteres
                cmp ah,1             ;si le caractere saisi n'est pas un chiffre
                je error             ;alors on signale une erreur
                inc di               ;sinon on incremente la position dans la zone
                loop boucle    
                
                jmp conversion       ;si tous les caracteres (hormis le signe) sont des chiffres
                                     ;alors on va vers la conversion  
 
        error:  mov ah,9             ;Sinon:
                lea dx,erreur        ;Fonction d'affichage: erreur
                int 21h              ;
                jmp lire             ;
                 
    
;CONVERSION ASCII EN NOMBRE DECIMAL

 conversion:    mov dx,ind
                add dl,bl
                dec dx               ;construction de l'indice max
                mov max,dx
                             
                mov cx,0000h         ;initialisation du compteur de boucles
                mov di,1             ;position de l'octet qui correspond a la longueur de la chaine
                mov cl,b.zone[di]    ;longueur de la chaine dans cl
                mov si,ind           ;initialisation de la position de parcours de la zone
                
                                 
                ;boucle qui convertit caractere par caractere 
       convert: mov al,b.zone[si]      ;on affecte un element de la zone
                sub al,30h           ;on soustrait 30 pour transformer le code ascii en decimal
                mov b.zone[si],al      ;sauvegarde des octets en decimal dans la zone de lecture
                inc si               ;incrementation de la position pour traiter le caractere suivant
                loop convert         
                
                
                
            
;VERIFICATION DE L'INTERVALLE [-32768,32767]

;===>LA VERIFICATION SE FAIT EN VALEUR ABSOLUE (chiffre par chiffre)
               
                mov di,ind           ;indice de parcours de la zone
                dec di               ;on decremente sinon on aura une boucle en plus dans "pour"
                mov si,-1D           ;initialisation du si 
                
                ;si le signe negatif a ete introduit
                
                cmp signe,2Dh    
                je negatif
                
                ;si il n'y a pas introduction de signe
                cmp signe,00h
                je  notsign
                
                ;si le signe + a ete introduit
          
         positif:
    
                mov ah,00h           ;initialisation de l'accumulateur
          pour1:inc di               ;Pour di allant de 1er chiffre au max (dernier)
                cmp di,max           ;
                ja conversionH       ;on va vers la conversion en Hexa
                
                inc si               ;indice de parcours de l'inf
                mov al,b.zone[di]    ;comparaison octet par octet 
                cmp al,sup[si]       
                ja limited           ;si c'est superieur on affiche une erreur
                je pour1             ;si c'est egal on continue la comparaison
                jl conversionH       ;si c'est inferieur c'est dans les normes( on peut alors convertir en hexa)
                
                
                
                
   negatif:     mov ax,di
                mov di,0001h         ;
                cmp b.zone[di],06h   ; si le nombre de position est inferieur a 6
                jl  conversionH      ; on n'a pas atteint la limite
                
                
                mov di,ax             
                mov ah,00h           ;
      pour:     inc di               ;Pour di allant de 1er chiffre au max (dernier)
                cmp di,max           ;
                ja stop              
                
                inc si               ;
                mov al,b.zone[di]    ; meme boucle que precedemment sauf que cette fois 
                cmp al,inf[si]       ; c'est avec l'inf (signe negatif)
                ja limited           ;
                je pour              ;
                jl conversionH       ;  conversion hexa
        
        ;Cas particulier: comme nous avons decide de faire la comparaison
                        ; en valeur absolue, alors la valeur |-32768| ne peut pas etre
                        ; etre traitee sur 16 bits donc on la traite a part
                               
      stop:            
                mov nombre,8000h               
                jmp fini
                
        ; Si le nombre est non signe on effectue trois traitements:
        ;         1) si la longueur est >5 donc c'est directement une erreur 
        ;         2) si la longueur est =5 donc on revient au cas ou le signe '+' est saisi
        ;         3) si la longueur est <5 donc le nombre est dans les normes
        
        
    notsign:    mov di,0001h         ;
                cmp b.zone[di],06h   ;si la longueur=6 on signale directement l'erreur
                je caspar            ;
                cmp b.zone[di],05h           ;si la longueur=5 on compare avec le sup
                je positif           ;sinon c'est dans les normes
                jmp conversionH 
                
    caspar:     mov di,0002h         ;
                cmp b.zone[di],00h   ; ce cas particulier traite un nombre de type :00000x
                je conversionH       ;
                       

   limited:     mov ah,9             ;
                lea dx,erreur        ; Message d'erreur
                int 21h              ;
                jmp lire             ;
                
;CONVERSION EN HEXADECIMAL

conversionH:                      
                mov di,ind           
                                
                mov cx,0010D         ;indice dans la zone 
                mov ah,00h
                mov al,b.zone[di]    ;initialisation de  la puissance
                
       refaire: cmp di,max
                je suite  
                mul cx               ;initialisation du compteur de boucles
                inc di               ;compteur de la puissance de 10     
                add al,b.zone[di]
                adc ah,00h
                jmp  refaire         ;si la puissance est nulle alors on fait directement l'ajout sans calculer la puissance    
                
          suite:mov nombre,ax 
                
                        
                
finlirenb:      ;conversion en ca2 dans le cas ou c'est negatif
                                  
                cmp signe,2Dh        ;  on verifie si le signe est negatif
                jne fini             ;
                mov si,-1H           ;alors on effectue le ca2
                imul si              ;
                mov nombre,ax        ;on range le resultat dans une case memoire dont l'etiquette est nombre     
                
 fini:                        
                                 
                ret
lirenb          endp
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

;------------------------------- 2e PROCEDURE DU TP 1 ----------------------------------    
ecrit_tab proc near
;Ecriture du contenu
    
                
    ECRIT:
    mov bx,10
    mov cx, 0
    mov si, '+' 
    
    add ax, 0            ; On effectue une addition pour voir si les flags sont positionnes
    jns EMPILEMENT       ; Si le nombre est positif, on le traite normalement, sinon on continue le cas special
    
    mov si, -1h 
    imul si             ; On multiplie le nombre negatif par (-1) et on traite le nombre positif normalement
    mov si,'-'  
    
    
     
    ; TRAITEMENT CAS GENERAL :
   
    EMPILEMENT: 
    
    mov dx,0h           ; A chaque iteration dx doit etre a zero pour contenir le reste de la division
    div bx
    add dl,30h          ; Application de la formule pour trouver le code ascii hexadecimal du chiffre
    push dx             ; On sauvegarde dans la pile la valeur de dx pour la re-utiliser
    inc cx
    cmp ax,9h           ; A partir du moment ou ax est inferieur ou egal a 9 il ne sert a rien de diviser,
    ja EMPILEMENT       ; puisque c'est la valeur elle-meme que l'on prendra.
     
    add al,30h          ; On gere le dernier reste en dehors de la boucle
    push ax    
    inc cx
    
    mov [di],si         ; On genere le signe "-" ou "+"
             
     
    DEPILEMENT: 
    
    pop ax
    inc di
    mov [di],al
    loop DEPILEMENT  
                
    mov ah,9
    lea dx,tab_ascii
    int 21h

    ret
    
ecrit_tab endp  
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
;;Rend 1 si le caractere est un operateur
operateur       proc   near
                mov    ah,0  
                cmp    al,'+'
                je     vrai
                cmp    al,'-'
                je     vrai
                cmp    al,'*'
                je     vrai
                cmp    al,'/'
                je     vrai  
                jmp    finop
vrai:           mov    ah,1
finop:          ret
operateur       endp
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
;procedure de lecture de l'operateur 

lireop         proc near
                
                mov ah,9
                lea dx,oper
                int 21h 
                
                mov si,2
                  
                                
        lire2:  mov ah,10            ;
                lea dx,zone2         ;Fonction de lecture vers une zone memoire
                int 21h              ; 
                
                
                mov al,b.zone2[si]
                mov operation,al
                call operateur
                
                cmp ah,1
                je  true 
                
                cmp al,'='
                je  finexp
                
                mov ah,9             ;
                lea dx,erreur2       ;Fonction d'affichage: erreur
                int 21h              ;
                jmp lire2            ;
                
    finexp:     inc arret
    true:       ret
lireop          endp
               
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
;on utilise des double mots car la multiplication peut causer un debordement sur 32 bits
;mais on pourrait par la suite avoir une division (resultat sur 16bits) qui est correct

addition        proc    near
                mov     bp,sp 
               
                mov     bx,[bp+2]    ;acces au nombre 2
                mov     si,[bx]      ;1er octet du nombre2
                mov     di,[bx+2]    ;2eme octet du nombre 2
                
                mov     bx,[bp+4]    ;acces au nombre1
                mov     ax,[bx]      ;1er octet du nombre1
                mov     dx,[bx+2]    ;2eme octet du nombre 1
                
                add     ax,si        ;operation d'addition 
               
                
                ;limite positive 32767
                jo     add_err   ;soit c'est un overflow(negatif au lieu de positif et vice versa)
                adc     dx,0     ;soit c'est un depassement de capacite sur 32 bits
                cmp     dx,0
                jne     add_err
                                                                                            
                mov     [bx],ax      ;stockage dans nombre 1  
                mov     [bx+2],dx
                  
                jmp     finadd   
      
                
        add_err:lea     dx,addit_err
                mov     ah,9
                int     21h 
                inc     veriferr
                
     finadd:    ret     4
addition        endp  

  
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
                    
soustraction    proc    near
                mov     bp,sp
                
                mov     bx,[bp+2]    ;nombre 2
                mov     si,[bx]
                mov     di,[bx+2]
                
                
                mov     bx,[bp+4]    ;nombre 1
                mov     ax,[bx]
                mov     dx,[bx+2]
                
                sub     ax,si        ;soustraction
              
                
                  ;limite negative -32768
                jo      sub_err
                cmp     dx,0   
                adc     dx,0
                jne     sub_err
                                                                                            
                mov     [bx],ax      ;stockage dans nombre 1  
                mov     [bx+2],dx
                  
                jmp     finsous   
      
                
        sub_err:lea     dx,sous_err
                mov     ah,9
                int     21h 
                inc     veriferr
               
                
       finsous:  ret     4
soustraction    endp   
  
;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
               
;---------------------------------------------------------------------------------------------


multiplication  proc
                mov     signe,0
                mov     bp,sp  
                
                mov     bx,[bp+2]   ;
                mov     ax,[bx]     ;les deux octets du nombre2 dernierement lu
                mov     dx,[bx+2]   ;
                
                ;TESTS :
                
                
                add     ax,0        ;verification du signe
                jns     suitepos
                
                mov     signe,1     ;complement a deux si le signe est negatif
                not     ax 
                not     dx
                add     ax,1
                adc     dx,0

    suitepos:   
                mov     si,ax       ;rangement du nombre 2 apres traitement
                mov     di, dx
                
                mov     bx,[bp+4]   ;octets du nombre1
                                    ;
                mov     ax,[bx]     ;
                mov     dx,[bx+2]   ;
                
                                                          
                cmp     dx,0
                jne     mulerror
                
                add     ax,0        ;signe du 1er nombre
                jns     suitepos2
                                
                xor     signe,1     ;signe du produit
                not     ax          ;complement a deux du nombre 2 s'il est negatif
                not     dx
                add     ax,1
                adc     dx,0  
                
                
                
                
             
    suitepos2:  
                mov     di,ax
                
                mul     si
                jo      mulerror 
                adc     dx,0
                cmp     dx,0
                jne      mulerror
                
                mov     ax,di
                sub     di,di
                     
                imul    dix
                add     al,virgule
                
                mov     di,dx       ; on gere la virugle
                sub     dx,dx
                
                imul    si
                
                push    ax
                push    dx
                
                sub     dx,dx
                sub     ax,ax
                
                mov     ax, di
                imul    si 
                
                sub     dx,dx
                
                pop     dx
                add     dx,ax
                
                sub     ax,ax
                pop     ax 
                

                
                div     dix
                mov     virgule,dl 
                
                   
                
                cmp     signe,1
                jne     rangement
                not     ax          ;complement a 2 si le produit est negatif
                not     dx
                add     ax,1
                adc     dx,0 
                jmp     rangement
                
mulerror:       mov ah,9
                lea dx,erreurcapacite
                int 21h
                inc veriferr
                jmp finmult2 
                 
                          
rangement:    

            mov     bx,[bp+4]  
            mov     [bx],ax     ;rangment du resultat dans nombre1  
                                               

 finmult2:               
                ret     4
multiplication       endp 

;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
division proc near
        
        mov     signe,0
        mov     bp,sp  

        
        mov     bx,[bp+2]
        mov     si,[bx]     ; Diviseur dans di:si
        mov     di,[bx+2]
        
        add     si,0
        js      inf_zero
        jmp     nul
          
    inf_zero: 
        sub dx,dx
        mov signe,1
        not si
        add si,1
        
        
	;Cas d'une division par 0 
	nul:
		cmp si,0
		jne non_nul
		cmp di,0
		jne non_nul         ; On confirme que di est bien a zero
		
		lea dx,div_zero
		mov ah,9
		int 21h
		
		jmp fin_div2
		
	;Si les operandes sont conformes	
	non_nul:
		
		mov cx, 0
		
		mov     bx,[bp+4]   
        mov     ax,[bx]     ; Dividende dans dx:ax     
        mov     dx,[bx+2]
		
	
	    cmp     dx,0
	    je      var_16
	    
	    add     dx,0
	    js      inf_zero2
	    jmp     cas_general 
	
	var_16:
	    add ax,0
	    jns cas_general
	    
	inf_zero2:
	    xor     signe,1
	    not     ax
	    not     dx
	    add     ax,1
	    adc     dx,0
	    
		
	;On va se baser sur un cas general (32 bits / 16 bits). Meme si dx est nul, ca ne fausse pas le resultat
	 
	cas_general:		
 		mov  di, ax         ; on enregistre le mot faible  
    
        mov  ax,dx          ; preparation du mot fort a la divison
        sub  dx,dx                                              
    
        idiv si 
           
        mov  cx,ax          ; on sauvegarde le quotient du mot fort
        mov  ax,di          ; ax := (mot faible)
        sub dx,dx
        
        idiv si
    
        
        xchg cx,dx          ; DX:AX : Quotient de la division.  CX : Reste.  
        
        cmp  signe,1
        jne  debut_virgule
        
     result_neg:
        not     ax
	    not     dx
	    add     ax,1
	    adc     dx,0 
        
     
    ;Gestion de la virgule (On ne prendra qu'un seul caratere) 
    debut_virgule:
        push dx
        push ax 
        sub di,di
        
        sub ax, ax
        mov ax, cx
        imul dix
        
        idiv si
        sub dx,dx
        
    un_chiffre:
        cmp ah,0
        jne ajust      ; on divise jusqu'a ce que ax ne soit plus que sur une seule position (000X) 
        
        imul dix
        cmp ah,0            ; on multiplie par dix pour tester si al deborde sur ah. Dans ce cas il n'est pas encore sur une position
        jne ajust 
        
        idiv dix
        jmp suite_vir
         
            
    ajust:
        idiv dix
        jmp un_chiffre    
		
    
    suite_vir:
        sub ah,ah
        add al,virgule
        cmp ah,0           ; la variable "virgule" ne doit varier que de 0 a 9.
        jne debor          ; si (al > 9) ou (ah <> 0) alors il y a debordement, on doit re-ajuster virgule et ajouter la retenue au quotient.
        cmp al,9h
        jg  debor 
        jmp fin_div
        
   debor:
        sub ax,dix
        inc di
        cmp ax,9
        jg debor
        
        
        
fin_div:    
            mov virgule, al
            sub ax,ax
            sub dx,dx
            
            
            pop ax          ; on reprend le quotient
            pop dx
            add ax,di     
            
            
            mov bx,[bp+4]
            mov [bx],ax    ; Le quotient DX:AX est mis dans la variable nombre1
            mov [bx+2],dx
		
fin_div2:
		
		ret 
division endp

;°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
     
calcul          proc near 
                
                ;1er calcul
                call lirenb
                mov  ax,nombre
                mov  nombre1,ax         ;lecture et empilement du 1er nombre
                push offset nombre1
                
                mov  bl,01h
                call lireop             ;lecture de l'operateur
                cmp bl,arret            ;condition d'arret si l'operateur est "="
                je  ftq 
                
                call lirenb 
                mov  ax,nombre          ;lecture et empilement du 2eme nombre
                mov  nombre2,ax
                push offset nombre2
                
                cmp operation,"+"
                je  addit 
                
                cmp operation,"-"
                je  soustr   
                
                cmp operation,"*"
                je multipl
                
                cmp operation,"/"
                je divis
        
        boucler:
                push offset nombre1 
                
                mov  bl,01h 
                call lireop 
                cmp bl,arret
                je  ftq 
                
                call lirenb
                mov  ax,nombre
                mov  nombre2,ax
                push offset nombre2
                
                cmp operation,"+"
                je  addit
                cmp operation,"-"
                je  soustr
                cmp operation,"*"
                je multipl
                cmp operation,"/"
                je divis
                          
    addit:      call addition
                cmp veriferr,0
                jne fincalcul
                jmp boucler
    soustr:    
                call soustraction 
                cmp veriferr,0
                jne fincalcul
                jmp boucler    
                
    multipl:    call multiplication 
                cmp veriferr,0
                jne fincalcul
                jmp boucler
    
    divis:      call division 
                cmp veriferr,0
                jne fincalcul
                jmp boucler 
           
           ftq: 
                lea dx,resul
                mov ah,9
                int 21h
                   
                lea di,tab_ascii
                mov ax,nombre1

                
                call ecrit_tab  
                
                mov dl,'.'
                mov ah,2            
                int 21h
                
                sub dl,dl
                mov dl,virgule      ; Affichage partie decimale
                add dl,30h
                
                mov ah,2
                int 21h
                
       fincalcul:  
                mov sp,ipretour
                ret         
         calcul endp    


;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²² 
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²                       ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²   PROGRAMME PRINCIPAL ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²                       ²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²  
;²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²           
debut:          
                mov  ax,s_donne
                mov  ds,ax 
                 
                
                ;message d'accueil
                mov  ah,9
                lea  dx,graphis
                int  21h 
                
                mov  ah,9
                lea  dx,accueil
                int  21h 
                
                ;procedure de calcul (lecture,traitement, ecriture)
                mov  ipretour,sp  ;sauvegarde du l'ip de retour
                call calcul

                ;procedure de sortie               
                call arreter                                
s_code          ends 
                end     debut 