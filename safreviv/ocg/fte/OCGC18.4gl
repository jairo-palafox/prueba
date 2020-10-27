#######################################################################
#Modulo              => OCG                                           #
#Programa            => OCGC18                                        #
#Objetivo            => PANTALLAS PARA CONSULTA DE SITUACIONES        #
#Autor               => JOSÉ EDUARDO VENTURA                          #
#Fecha inicio        => 11 de oCTUBRE de 2017                         #
#######################################################################

DATABASE safre_viv

GLOBALS
   DEFINE p_usuario        CHAR(20)
   DEFINE p_tpo_ejecucion  SMALLINT
   DEFINE p_s_titulo       CHAR(20)
   DEFINE v_situacion      SMALLINT
   DEFINE v_subproceso     SMALLINT
   DEFINE v_bnd_subproceso SMALLINT
   DEFINE v_cnt_sit        SMALLINT
   DEFINE v_bnd_situacion  STRING
   DEFINE v_bnd_msj        SMALLINT
   
END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parámetro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC18.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   OPEN WINDOW OCGC18 WITH FORM "OCGC181"

   INPUT BY NAME v_situacion,
                 v_subproceso ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT
   
      LET v_bnd_subproceso = 0
      LET v_bnd_situacion  = 0
      LET v_bnd_msj        = 0

      IF v_situacion IS NOT NULL THEN

         SELECT COUNT(*)
           INTO v_cnt_sit
           FROM cat_ocg_situacion
          WHERE situacion = v_situacion

         IF v_cnt_sit >= 1 THEN
            LET v_bnd_situacion = 1
         ELSE
            CALL fn_mensaje ("Archivo","Situacion ingresada no existe","information")
            LET v_bnd_situacion = 0
            LET v_bnd_msj = 1
         END IF
      ELSE
         LET v_bnd_situacion = NULL
      END IF

      IF v_subproceso IS NULL THEN
         IF v_bnd_msj = 0 THEN
            CALL fn_mensaje ("Archivo","Debe ingresar subproceso a consultar","information")
         END IF
      ELSE
         IF v_subproceso = 1 OR
            v_subproceso = 2 OR
            v_subproceso = 3 OR
            v_subproceso = 4 OR
            v_subproceso = 5 THEN
            LET v_bnd_subproceso = 1
         ELSE
            LET v_bnd_subproceso = 0
         END IF
      END IF

      IF v_bnd_subproceso = 1 THEN
         IF v_bnd_situacion IS NULL OR 
            v_bnd_situacion = 1 THEN
            CALL fn_consulta_subproceso()
         END IF
      END IF

      IF v_bnd_subproceso = 0 AND
         v_subproceso     IS NOT NULL THEN
         CALL fn_mensaje ("Archivo","No existen registros para subproceso ingresado","information")
      END IF

      --END INPUT

   ON ACTION CANCEL

      EXIT INPUT

      END INPUT
   CLOSE WINDOW OCGC18

END MAIN

FUNCTION fn_consulta_subproceso()

   DEFINE v_cadena  STRING
   DEFINE v_qry_sit STRING
   DEFINE a          SMALLINT 

   DEFINE arr_situacion DYNAMIC ARRAY OF RECORD
      situacion SMALLINT
   END RECORD

   LET v_cadena = ' '

   IF v_bnd_situacion = 1 THEN
      LET v_cadena = v_situacion
   ELSE
      LET v_qry_sit = " SELECT situacion ",
                        " FROM cat_ocg_situacion"

      DECLARE cur_situacion CURSOR FROM v_qry_sit

      LET a = 1

      FOREACH cur_situacion INTO arr_situacion[a].situacion
         LET a = a + 1
      END FOREACH

      CALL arr_situacion.deleteElement(a)

      FOR a = 1 TO arr_situacion.getLength()
         IF a = arr_situacion.getLength() THEN
            LET v_cadena = v_cadena,arr_situacion[a].situacion
         ELSE
            LET v_cadena = v_cadena,arr_situacion[a].situacion,','
         END IF
      END FOR

      CALL fn_mensaje ("Archivo",v_cadena,"information")
      
   END IF

END FUNCTION