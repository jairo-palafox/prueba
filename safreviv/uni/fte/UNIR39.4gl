--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:19/Nov/2015
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR39                                                        #
#Objetivo     => Lanzado reverso preliquidación                                #
#Fecha inicio => 19/Nov/2015                                                   #
################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_s_titulo       STRING,
       p_folio          DECIMAL(9,0),
       v_folio_lote     DECIMAL(9,0),
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
       r_bandera        SMALLINT,
       v_QryTxt         STRING,
       p_titulo         STRING,
       p_mensaje        STRING,
       v_resultado      SMALLINT,
       isam_err         INTEGER,
       err_txt          CHAR(200)

   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR39.log")

   CALL fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod) 
        RETURNING r_bandera

   IF (r_bandera = 0) THEN
      CALL fn_reversa_preliquidacion(p_folio, g_proceso_cod, g_opera_cod)
           RETURNING r_bandera

      IF(r_bandera = 0)THEN
         LET v_QryTxt = "EXECUTE FUNCTION fn_uni_reversa_preliquida_nvo_mod(?,?,?)"
         PREPARE prp_reverso_preliq FROM v_QryTxt
         EXECUTE prp_reverso_preliq USING p_folio, --unificación
                                          g_pid,
                                          g_proceso_cod
                                    INTO  v_resultado,
                                          isam_err,
                                          err_txt
         IF (v_resultado = 0) THEN 
            CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
                 RETURNING r_bandera

            IF(r_bandera = 0)THEN
               DISPLAY "El reverso se realizó con éxito"
               DISPLAY "Operación lista para volver a generarse."
            ELSE
               DISPLAY fn_recupera_inconsis_opera(r_bandera)
            END IF
         ELSE
            DISPLAY "   --- ERROR --- "
            DISPLAY "   Ocurrió un error al procesar el reverso de la integración"  
            DISPLAY "   Resultado : " , v_resultado
            DISPLAY "   ISAM      : " , isam_err
            DISPLAY "   Error     : " , err_txt
         END IF
      ELSE
         DISPLAY fn_recupera_inconsis_opera(r_bandera)
      END IF
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF   
   
   LET p_titulo = "Finalización de proceso - REVERSO PRELIQUIDACION"
   
   LET p_mensaje = "   Finalización de proceso - REVERSO PRELIQUIDACION","\n",
                  " \n",
                  "   Folio: "||p_folio,"\n",
                  " \n",
                  "   Fecha de inicio: "||TODAY,"\n",
                  " \n",
                  "   Hora           : ",CURRENT HOUR TO SECOND,"\n"

END MAIN
