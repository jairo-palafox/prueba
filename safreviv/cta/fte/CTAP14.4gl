#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#Modulo            => CTA                                                     #
#Programa          => CTAP14                                                  #
#Objetivo          => PROGRAMA PARA GENERAR INFORMACION ESTADISTA PARA DWH    #
#Fecha Inicio      => NOVIEMBRE-2017                                          #
###############################################################################
DATABASE safre_viv

#Parametros generales del proceso
DEFINE p_pid                      DECIMAL(9,0)
DEFINE p_proceso_cod              SMALLINT    
DEFINE p_opera_cod                SMALLINT    
DEFINE p_usuario_cod              CHAR(20)    
DEFINE p_nombre_archivo           CHAR(40)    
DEFINE p_folio                    DECIMAL(9,0)


DEFINE v_proceso_desc             CHAR(40)
DEFINE v_opera_desc               CHAR(40)

MAIN
   DEFINE r_resultado_opera               INTEGER

   DEFINE v_fecha_actual                  DATE
   

   DEFINE v_query                         STRING

   DEFINE v_error                         INTEGER
   DEFINE v_isam                          INTEGER
   DEFINE v_descripcion                   CHAR(200)

   

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod

   #Encabezado para el archivo de monitoreo
   LET v_fecha_actual = TODAY
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

            DISPLAY ""
            DISPLAY ""
            DISPLAY "EXTRACCIÓN DE INFORMACIÓN DE  DISPOSICIONES "
            LET v_query = "EXECUTE PROCEDURE sp_dwh_extractor_retiros() "
            PREPARE prp_ext_indica FROM v_query
            EXECUTE prp_ext_indica  INTO v_error, v_isam, v_descripcion
            IF v_error <> 0 THEN
               DISPLAY "SACI-EXCEPCION EJECUCION sp_dwh_extractor_retiros"
               DISPLAY "ERROR       ",v_error
               DISPLAY "ISAM ERROR  ",v_isam
               DISPLAY "DESCRIPCION ",v_descripcion
               DATABASE safre_viv
               CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
               RETURNING r_resultado_opera
            ELSE
               DATABASE safre_viv
               CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
               RETURNING r_resultado_opera
               IF(r_resultado_opera <> 0)THEN         
                  DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
                  CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
                  RETURNING r_resultado_opera
               END IF
               DISPLAY ""
               DISPLAY "*******************************************************************"
               DISPLAY "SACI-DWH TERMINO CORRECTAMENTE LA EXTRACCIÓN DE ACLARATORIOS"
               DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
               DISPLAY " HORA               : ",TIME(CURRENT)
               DISPLAY "*******************************************************************"
           END IF  
        
END MAIN