    ;Archivo:	    Lab6.s
    ;Dispositivo:   PIC16F887
    ;Autor:	    Brandon Garrido
    ;Compilador:    pic-as(v2.31), MPLABX V5.45
    ;
    ;Programa:	    Temporizadores con TIMER1 y TIMER2
    ;Hardware:	    Displays 7 seg en puerto C, transistores en puerto D,
    ;		    led titilante en puerto E
    ;Creado: 23 mar 2021
    ;Última modificación: 27 mar, 2021
    


PROCESSOR 16F887
#include <xc.inc>

; CONFIG1
  CONFIG  FOSC = INTRC_NOCLKOUT ; Oscillator Selection bits (RC oscillator: CLKOUT function on RA6/OSC2/CLKOUT pin, RC on RA7/OSC1/CLKIN)
  CONFIG  WDTE = OFF            ; Watchdog Timer Enable bit (WDT disabled and can be enabled by SWDTEN bit of the WDTCON register)
  CONFIG  PWRTE = ON            ; Power-up Timer Enable bit (PWRT enabled)
  CONFIG  MCLRE = OFF           ; RE3/MCLR pin function select bit (RE3/MCLR pin function is digital input, MCLR internally tied to VDD)
  CONFIG  CP = OFF              ; Code Protection bit (Program memory code protection is disabled)
  CONFIG  CPD = OFF             ; Data Code Protection bit (Data memory code protection is disabled)
  CONFIG  BOREN = OFF           ; Brown Out Reset Selection bits (BOR disabled)
  CONFIG  IESO = OFF            ; Internal External Switchover bit (Internal/External Switchover mode is disabled)
  CONFIG  FCMEN = OFF           ; Fail-Safe Clock Monitor Enabled bit (Fail-Safe Clock Monitor is disabled)
  CONFIG  LVP = ON              ; Low Voltage Programming Enable bit (RB3/PGM pin has PGM function, low voltage programming enabled)

; CONFIG2
  CONFIG  BOR4V = BOR40V        ; Brown-out Reset Selection bit (Brown-out Reset set to 4.0V)
  CONFIG  WRT = OFF             ; Flash Program Memory Self Write Enable bits (Write protection off)
    
 
;-------------------------------------------------------------------------------
; Macros
;-------------------------------------------------------------------------------    
    
reiniciar_tmr0 macro ; macro para reutilizar reinicio de tmr0
    movlw 252 ; valor de n para (256-n)
    movwf TMR0 ; delay inicial TMR0
    bcf T0IF
endm
  
reiniciar_tmr1 macro	; reiniciar de Timer1
    Banksel PORTA   ; banco 0
    movlw   0x85   ; cargar al registro W el valor inicial del tmr1 high
    movwf   TMR1H   ; cargar timer 1 high
    movlw   0xEE    ;cargar al registro W el valor inicial del tmr1 low
    movwf   TMR1L    ; cargar timer 1 low
    bcf	    TMR1IF    ; limpiar bandera de interrupción	 timer1
endm
    
reiniciar_tmr2 macro	; reiniciar de Timer2
    Banksel PORTA   ; banco 0
    movlw   0xFF   ; cargar al registro W el valor inicial del tmr2
    movwf   PR2    ; cargar timer 2
    bcf	    TMR2IF    ;limpiar bandera de interrupción	timer2	
endm
;-------------------------------------------------------------------------------
; Variables 
;-------------------------------------------------------------------------------
PSECT udata_bank0 ;common memory
    var: DS 1 ;1 byte -> para bucle
    banderas: DS 1 ;1 byte -> para contador de display timer0
    ;banderas2: DS 1
    nibble: DS 2; variables para contador hexademimal
    display_var: DS 2	
    cont:DS 1
    
PSECT udata_shr ;common memory
    W_TEMP: DS 1 ;1 byte
    STATUS_TEMP: DS 1; 1 byte
    
;-------------------------------------------------------------------------------
; Vector reset
;-------------------------------------------------------------------------------
PSECT resVect, class=CODE, abs, delta=2
ORG 00h
resetVec:
    PAGESEL main
    goto main
    

PSECT intVect, class=CODE, abs, delta=2
;-------------------------------------------------------------------------------
; Vector de interrupción
;-------------------------------------------------------------------------------
ORG 04h ;posición 0x0004

push: ;Preservar los valores de W y las banderas
    movwf W_TEMP
    swapf STATUS, W
    movwf STATUS_TEMP 
    
isr: ; rutina de interrupción
    btfsc   T0IF ; verifica si la bandera de interrupcion tmr0 esta levantada
    call    int_tmr0	;subrutina de interrupción de timer0
    btfsc   TMR1IF; verifica si la bandera de interrupcion tmr1 esta levantada
    call    int_tmr1	; subrutina de interrupción de timer1
    btfsc   TMR2IF ; verifica si la bandera de interrupcion tmr2 esta levantada
    call    int_tmr2	; subrutina de interrupción de timer2

pop: ; para re-obtener los valores de W y de las banderas de status
    swapf STATUS_TEMP, W
    movwf STATUS
    swapf W_TEMP, F
    swapf W_TEMP, W
    retfie ; finalizar interrupción
    

;-------------------------------------------------------------------------------
; Sub rutinas para interrupciones
;-------------------------------------------------------------------------------

int_tmr2:
    reiniciar_tmr2
    incf PORTE ; prender led parpadeante implementando 1 bit del PORTE
    
    return

int_tmr1:
    reiniciar_tmr1
    incf    PORTA ; incrementar contador en el PORTA con timer1
    return
    
int_tmr0: 
    reiniciar_tmr0
    clrf PORTD 
    ;verificar bit por bit el display encendido
    btfsc PORTE, 0
    return
    btfsc banderas,0
    goto display_1; multiplexado de displays con timer0
    
    
display_0: ; display del nibble menos significativo
    movf display_var+0,W
    movwf PORTC
    bsf PORTD,0 ;habilitar pin 0 D para encender display 0
    goto next_display 

display_1: ; display del nibble más significativo
    movf display_var+1,W
    movwf PORTC
    bsf PORTD,1 ;habilitar pin 1 D para encender display 1
    goto next_display
    
next_display:; subrutina para ir iterando entre cada uno de los display
    movlw 1
    xorwf banderas,F 
    return
    
;-------------------------------------------------------------------------------
; Código Principal
;-------------------------------------------------------------------------------
PSECT code, delta=2, abs
ORG 100h ;Posición para el código
 
tabla: ; tabla de valor de pines encendido para mostrar x valor en el display
    clrf PCLATH
    bsf PCLATH, 0 ; PCLATH = 01 PCL = 02
    andlw 0x0f ; para solo llegar hasta f
    addwf PCL ;PC = PCLATH + PCL + W
    retlw 00111111B ;0
    retlw 00000110B ;1
    retlw 01011011B ;2
    retlw 01001111B ;3
    retlw 01100110B ;4
    retlw 01101101B ;5
    retlw 01111101B ;6
    retlw 00000111B ;7
    retlw 01111111B ;8
    retlw 01101111B ;9
    retlw 01110111B ;A
    retlw 01111100B ;B
    retlw 00111001B ;C
    retlw 01011110B ;D
    retlw 01111001B ;E
    retlw 01110001B ;F 

;-------------------------------------------------------------------------------
; Configuraciones
;-------------------------------------------------------------------------------   
main:
    call config_io ; PORTA salida; RB7 y RB2 como input
    call config_reloj ;4MHz
    ;configuraciones de los timers en modo temporizador
    call config_tmr0  ;
    call config_tmr1  ;
    call config_tmr2  ;
    call config_int_enable ; configurar banderas de interrupción
        
loop:
   movf PORTA, W   ; pasar valor de porta a var temporal display hexadecimal
   movwf var
     
   call separar_nibbles 
   call preparar_displays  
   
   goto loop	   ;loop forever

;-------------------------------------------------------------------------------
; Sub rutinas
;-------------------------------------------------------------------------------
separar_nibbles:
    movf var,W 
    andlw 0x0f ;deshabilitar nibble op
    movwf nibble
    swapf var,W ;swapear nible y luego sumar 1
    andlw 0x0f
    movwf nibble+1
    
    return
   
preparar_displays:
    movf nibble,W
    call tabla
    movwf display_var
    movf nibble+1,W
    call tabla
    movwf display_var+1
   
    return

;-------------------------------------------------------------------------------
; Subrutinas de configuración
;-------------------------------------------------------------------------------
config_io:
    banksel ANSEL ;Banco 11
    clrf    ANSEL ;Pines digitales
    clrf    ANSELH
    
    banksel TRISA ;Banco 01
    clrf    TRISA
    clrf    TRISC    ;Display multiplexados 7seg 
    clrf    TRISD    ;Alternancia de displays
    bcf    TRISE, 0
    
    banksel PORTA ;Banco 00
    clrf    PORTA ;Comenzar contador binario en 0
    clrf    PORTC ;Comenzar displays en 0
    clrf    PORTD ;Comenzar la alternancia de displays en 0
    bcf    PORTE, 0
    return
    
config_int_enable:; INTCON
    Banksel PORTA
    bsf	GIE	; Se habilitan las interrupciones globales
    bsf	PEIE	
    bsf	T0IE    ; Se habilitan la interrupción del TMR0
    bcf	T0IF    ; Se limpia la bandera
    Banksel TRISA
    bsf	TMR1IE	; Se habilitan la interrupción del TMR1 Registro PIE1
    bsf	TMR2IE	; Se habilitan la interrupción del TMR2 Registro PIE1
    Banksel PORTA
    bcf	TMR1IF  ; Se limpia la bandera Registro PIR1
    bcf	TMR2IF  ; Se limpia la bandera Registro PIR1
    return  
   
config_reloj:	; Configuración de reloj interno
    Banksel OSCCON  ; Acceder al Bank 1
    bsf	    IRCF2
    bcf	    IRCF1
    bcf	    IRCF0   ; Configuración del oscilador a 1MHz
    bsf	    SCS	    ; Seleccionar el reloj interno
    return
    
    
;t=4 * (T_osc) * (256-n) (Preescaler) 
config_tmr0:
    banksel TRISA
    bcf T0CS ;habilitar reloj interno
    bcf PSA ; prescaler
    bsf PS2 
    bsf PS1 
    bsf PS0 ; PS = 111 (1:256)
    banksel PORTA
    
    reiniciar_tmr0
      
    return
    
 ;t=4 * (T_osc) * (63536-n) (Preescaler) 
config_tmr1:
    Banksel PORTA  
    bsf	    TMR1ON
    bcf	    TMR1CS ; Seleccion del reloj interno
    bsf	    T1CKPS1
    bsf	    T1CKPS0 ; PS a 1:8
    reiniciar_tmr1
    return
    
  ;t=4 * (T_osc) * (Preescaler) (PR2) (Postcaler)
config_tmr2:
    banksel PORTA
    bsf TMR2ON ; reloj interno
    
    bsf TOUTPS3
    bsf TOUTPS2
    bsf TOUTPS1
    bsf TOUTPS0;POSTCALEER (1:16)
    
    bsf T2CKPS1
    bsf TOUTPS0;PS (1:16)
 
    reiniciar_tmr2
      
    return
       
end