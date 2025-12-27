      ******************************************************************
      * Author: NICOLAS ALOY
      * Date:18/05/2025
      * Purpose: PRACTICA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAJERO-AUTOMATICO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 opcion  PIC 9(1).
       01 cantidad pic 9(5).
       01 contador PIC 9(2) VALUE 1.
       01 indice PIC 9(1) VALUE 1.
       01 cuentas OCCURS 2 TIMES.
       05 saldo-cuenta  PIC 9(5)  VALUE 0000.
       05 Historial-cuenta OCCURS 10 TIMES.
       10 tipo-transsaccion PIC x(10) VALUE SPACES.
       10 Monto-transaccion pic 9(5) VALUE 0.

       01 Entrada-Usuario PIC X(30).

       PROCEDURE DIVISION.

       BUCLE-ATH.
       INICIO-BANCO.
            DISPLAY "=================================================".
            DISPLAY "XXXXXXXxxxxxx CAJERO AUTOMATICO xxxxxxXXXXXXXXXXX".
            DISPLAY "=================================================".
            DISPLAY "1. INGRESAR".
            DISPLAY "2.RETIRAR".
            DISPLAY "3.CONSULTAR SALDO-CUENTA".
            DISPLAY "4. HISTORIAL-CUENTA".
            DISPLAY "5. SALIR-CUENTA".
            DISPLAY "6. TRANSFERENCIAS".
            DISPLAY "=================================================".
            ACCEPT Entrada-Usuario.

            MOVE Entrada-Usuario(1:1)TO opcion.

            EVALUATE OPCION
        WHEN 1
        PERFORM PROCEDIMIENTO-DEPOSITO
        WHEN 2
        PERFORM PROCEDIMIENTO-RETIRO
        WHEN 3
        PERFORM CONSULTA-SALDO
       WHEN 4
        PERFORM MOSTRAR-HISTORIAL
       WHEN 5
        DISPLAY "GRACIAS POR USAR EL CAJERO AUTOMATICO :D"
       WHEN 6
        PERFORM PROCEDIMIENTO-TRANSFERENCIA
       WHEN OTHER
        DISPLAY "OPCION INVALIDA"
        END-EVALUATE.
        GO TO BUCLE-ATH



       STOP RUN.


       PROCEDIMIENTO-DEPOSITO.
       DISPLAY "INTRODUZCA LA CANTIDAD DEL MONTO A INGRESAR".
       ACCEPT cantidad.
       COMPUTE saldo-cuenta(indice)= saldo-cuenta(indice)+cantidad.
       MOVE "INGRESOS" TO Monto-transaccion(indice,contador).
       MOVE cantidad to Monto-transaccion(indice,contador).
       ADD 1 to contador.
       IF contador > 10
       MOVE 1 to contador.
       DISPLAY "INGRESO REALIZADO CON EXITO SALDO ACTUAL: "
       saldo-cuenta(indice).


       PROCEDIMIENTO-RETIRO.
       DISPLAY "INTRODUZCA LA CANTIDAD A RETIRAR: "
       ACCEPT cantidad.
       IF cantidad> saldo-cuenta(indice) THEN
       DISPLAY "SALDO INSUFICIENTE"
       GO TO BUCLE-ATH.

       COMPUTE saldo-cuenta(indice)= saldo-cuenta(indice)-cantidad.
       MOVE "RETIRO" TO tipo-transsaccion(indice,contador).
       move cantidad TO Monto-transaccion(indice,contador).
       ADD 1 TO contador.
       DISPLAY "RETIRO REALIZADO CON EXITO SALDO ACTUAL: "
       saldo-cuenta(indice).

       CONSULTA-SALDO.
       DISPLAY "SALDO ACTUAL: " saldo-cuenta(indice).

       MOSTRAR-HISTORIAL.
       DISPLAY"HISTORIAL: "
        PERFORM VARYING CONTADOR FROM 1 BY 1 UNTIL CONTADOR > 10
        IF TIPO-TRANSSACCION(INDICE, CONTADOR) NOT = SPACES
        DISPLAY "TIPO: ", TIPO-TRANSSACCION(INDICE, CONTADOR)
        DISPLAY "SALDO: ", MONTO-TRANSACCION(INDICE, CONTADOR)
        END-IF
       END-PERFORM.


       PROCEDIMIENTO-TRANSFERENCIA.
       DISPLAY "INTRODUCIR CUENTA DESTINO(1 O 2)".
       ACCEPT opcion.
       if opcion not= 1 AND opcion not = 2
       DISPLAY " cuanta destino inexistente"
       ELSE
           IF opcion not EQUAL indice THEN
               DISPLAY " introduzca la cantida a transferir: "
               ACCEPT cantidad.
               IF cantidad > saldo-cuenta(indice) THEN
                   DISPLAY "SALDO INSUFICIENTE"
                   ELSE
       COMPUTE saldo-cuenta(indice) = saldo-cuenta(indice)- cantidad.
       DISPLAY"SALDO ACTUAL: " saldo-cuenta(indice).













       END PROGRAM CAJERO-AUTOMATICO.
