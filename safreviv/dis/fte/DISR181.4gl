#############################################################################
#Módulo          => DIS                                                     #
#Programa        => DISR181.4gl                                             #
#Objetivo        => Programa de Reverso Liquidacion de aportaciones         #
#                   subsecuentes sin adelanto.                              #
#Fecha Inicio    => 19/11/2015                                              #
#############################################################################
DATABASE safre_viv

DEFINE g_folio_liquida             LIKE glo_folio.folio,
       g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
       g_opera_cod                 LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nom_archivo               STRING

MAIN
   #Si se ha recibido parámetros se continua    
   IF (NUM_ARGS() > 0)THEN
      LET g_usuario_cod              = ARG_VAL(1)
      LET g_pid                      = ARG_VAL(2)
      LET g_proceso_cod              = ARG_VAL(3)
      LET g_opera_cod                = ARG_VAL(4)
      LET g_folio_liquida            = ARG_VAL(5)
      LET g_nom_archivo              = ARG_VAL(6)

      CALL STARTLOG(g_usuario_cod CLIPPED||".DISR181.log")
      CALL fn_reverso()
   END IF
END MAIN

FUNCTION fn_reverso()
   DEFINE r_sql_code SMALLINT
   DEFINE r_bandera  INTEGER
   DEFINE v_QryTxt   STRING

   DISPLAY ""
   DISPLAY ""
   DISPLAY ("##############################################################")
   DISPLAY "Inicio del Reverso de Liquidación de Aportaciones Subsecuentes"
   DISPLAY "Folio:"   ,g_folio_liquida

   CALL fn_reverso_liquidacion(g_folio_liquida) RETURNING r_sql_code

   IF r_sql_code = 0 THEN
      CALL fn_reversa_operacion(g_pid, g_proceso_cod, 4) 
      RETURNING r_bandera

      LET v_QryTxt = "\n UPDATE glo_folio",
                     "\n SET    status      = 1",
                     "\n WHERE  folio       = ",g_folio_liquida,
                     "\n AND    proceso_cod = ",g_proceso_cod,
                     "\n AND    opera_cod   = 2"
 
      PREPARE prp_update_estatus_folio FROM v_QryTxt
      EXECUTE prp_update_estatus_folio
      DISPLAY "Reverso de Liquidación de Aportaciones Subsecuentes Finalizado"
      DISPLAY ""
      DISPLAY ("##############################################################")
   ELSE
      DISPLAY ("##############################################################")
      DISPLAY ""
      IF r_sql_code = 1 THEN 
         DISPLAY("No existe el registro contable del proceso")
      END IF
      IF r_sql_code = 2 THEN 
         DISPLAY("La fecha de emisión es diferente al día de hoy")
      END IF
      IF r_sql_code = 3 THEN 
         DISPLAY("La póliza contable ya fue generada")
      END IF 
      DISPLAY ""
      DISPLAY ("##############################################################")
   END IF 

END FUNCTION
