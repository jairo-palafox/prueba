#######################################################################
#Modulo              => OCG                                           #
#Programa            => OCGC18                                        #
#Objetivo            => PANTALLAS PARA RELACIÓN LABORAL               #
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
   DEFINE v_nss            CHAR (11)
   DEFINE arr_rl DYNAMIC ARRAY OF RECORD
      nss        CHAR(11),
      f_alta     DATE,
      f_baja     DATE,
      f_proceso  DATE
   END RECORD

   DEFINE v_qry_rl STRING

   DEFINE a         SMALLINT
   DEFINE ch                        base.Channel
   DEFINE v_nom_arh                 STRING
   DEFINE v_ruta_envio              LIKE seg_modulo.ruta_envio
   DEFINE v_detalle                 STRING
   DEFINE v_nss_1                   CHAR(11)
   DEFINE v_f_alta                  DATE
   DEFINE v_f_baja                  DATE
   DEFINE v_mensaje                 STRING

   
END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parámetro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC19.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   OPEN WINDOW OCGC192 WITH FORM "OCGC192"

   INPUT BY NAME v_nss ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

   LET v_cadena = " "

   IF v_nss IS NOT NULL THEN
      LET v_cadena = " AND nss = ",v_nss
   END IF

      LET v_qry_rl = "
   SELECT first 32000 nss,
          f_alta,
          f_baja,
          f_proceso
     FROM ocg_relacion_laboral 
    WHERE 1 = 1 ",v_cadena

   PREPARE prp_rl FROM v_qry_rl
   DECLARE cur_rl CURSOR FOR prp_rl

   LET a = 1
   FOREACH cur_rl INTO arr_rl[a].*
      LET a = a+1
   END FOREACH

   CALL arr_rl.deleteElement(a)

   IF arr_rl.getlength() >= 1 THEN

      SELECT ruta_envio
        INTO v_ruta_envio
        FROM seg_modulo
       WHERE modulo_cod = 'ocg'

      LET v_nom_arh = v_ruta_envio CLIPPED ,"/consulta_rl",".txt"


   OPEN WINDOW OCGC191 WITH FORM "OCGC191"

   DISPLAY ARRAY arr_rl TO tab_rl.*

   ON ACTION archivo
      LET  ch = base.Channel.create()
      CALL ch.openFile(v_nom_arh,"w" )
      CALL ch.setDelimiter("|")

          FOR a = 1 TO arr_rl.getLength()

            LET v_nss_1  = arr_rl[a].nss
            LET v_f_alta = arr_rl[a].f_alta
            LET v_f_baja = arr_rl[a].f_baja

            LET v_detalle = v_nss_1,"|",
                            v_f_alta USING"yyyymmdd","|",
                            v_f_baja USING"yyyymmdd"

            CALL ch.writeLine([v_detalle])

         END FOR

         CALL ch.close()

         LET v_mensaje = "Archivo generado de forma correcta en : ",v_nom_arh
         CALL fn_mensaje ("Archivo",v_mensaje,"information")

      EXIT DISPLAY
      LET v_cadena = ""

   ON ACTION CANCEL
      EXIT DISPLAY
      LET v_cadena = ""

      END DISPLAY
   CLOSE WINDOW OCGC191
   ELSE
      CALL fn_mensaje("Alerta", "No se encontraron registros con parámetros ingresados ","stop")
   END IF

   ON ACTION CANCEL
      EXIT INPUT
      END INPUT
   
   CLOSE WINDOW OCGC192

END MAIN
