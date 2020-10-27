#######################################################################
#Modulo              => OCG                                           #
#Programa            => OCGC18                                        #
#Objetivo            => consulta histórica de transacciones           #
#Autor               => JOSÉ EDUARDO VENTURA                          #
#Fecha inicio        => 11 de oCTUBRE de 2017                         #
#######################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario        CHAR(20)
   DEFINE p_tpo_ejecucion  SMALLINT
   DEFINE p_s_titulo       CHAR(20)
   DEFINE v_f_proceso      DATE
   DEFINE v_cadena         STRING
   DEFINE v_nss            CHAR(11)
   DEFINE v_s_qry          STRING
   DEFINE a                INTEGER

   DEFINE arr_transaccion DYNAMIC ARRAY OF RECORD
      nss                 CHAR(11),
      cve_ent_financiera  CHAR(3),
      ssv97               char(17),
      periodo_pago        CHAR(10),
      f_pago              CHAR(10),
      concepto            CHAR(3)
   END RECORD

END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parámetro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC20.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   OPEN WINDOW OCGC201 WITH FORM "OCGC201"

   INPUT BY NAME v_nss ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

      LET v_s_qry = "SELECT first 32000 nss,
                            cve_ent_financiera,
                            ssv97,
                            periodo_pago,
                            f_pago,
                            concepto
                       FROM ocg_mig_transaccion
                      WHERE nss = '",v_nss,"'"

   PREPARE prp_transaccion FROM v_s_qry
   DECLARE cur_transaccion CURSOR FOR prp_transaccion

   LET a = 1

   FOREACH cur_transaccion INTO arr_transaccion[a].*
      LET a = a+1
   END FOREACH

   CALL arr_transaccion.deleteElement(a)

   IF  arr_transaccion.getLength() >= 1 THEN
      OPEN WINDOW OCGC202 WITH FORM "OCGC202"

      DISPLAY ARRAY arr_transaccion TO tab_transaccion.*

      END DISPLAY
      CLOSE WINDOW OCGC202
   ELSE
      CALL fn_mensaje("Alerta","No se encontraron registros con datos ingresados","stop")
   END IF

   ON ACTION CANCEL

   EXIT INPUT
   END INPUT

      CLOSE WINDOW OCGC201

END MAIN